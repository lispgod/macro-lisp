use proc_macro2::{Delimiter, TokenTree, TokenStream as TokenStream2};
use quote::{quote, quote_spanned};

use crate::helpers::{is_punct, is_ident, validate_type};
use crate::output::{LispOutput, verbatim_expr, build_block_stmts};
use crate::shared::{parse_visibility, parse_body_items};
use crate::expr::{eval_lisp_expr, eval_lisp_arg};

pub(crate) fn eval_if(tokens: &[TokenTree]) -> syn::Expr {
    if tokens.is_empty() { return verbatim_expr(quote! {}); }

    // Check for `if let` form: (if let (Pat = expr) then else)
    // Keep if-let as Verbatim for now (syn::ExprLet is complex)
    if is_ident(&tokens[0], "let") && tokens.len() >= 3 {
        if let TokenTree::Group(g) = &tokens[1] {
            if g.delimiter() == Delimiter::Parenthesis {
                let inner: Vec<TokenTree> = g.stream().into_iter().collect();
                // Find `=` separator
                if let Some(eq_pos) = inner.iter().position(|t| {
                    if let TokenTree::Punct(p) = t { p.as_char() == '=' } else { false }
                }) {
                    let pat: TokenStream2 = inner[..eq_pos].iter().cloned().collect();
                    let val = eval_lisp_arg(&inner[eq_pos + 1..]);
                    if tokens.len() >= 4 {
                        let then_branch = eval_lisp_arg(&tokens[2..3]);
                        let else_branch = eval_lisp_arg(&tokens[3..4]);
                        return verbatim_expr(quote! { if let #pat = #val { #then_branch } else { #else_branch } });
                    } else {
                        let then_branch = eval_lisp_arg(&tokens[2..3]);
                        return verbatim_expr(quote! { if let #pat = #val { #then_branch } });
                    }
                }
            }
        }
    }

    let cond = eval_lisp_arg(&tokens[0..1]);
    let then_block = if tokens.len() >= 2 {
        syn::Block {
            brace_token: syn::token::Brace::default(),
            stmts: build_block_stmts(vec![LispOutput::Expr(eval_lisp_arg(&tokens[1..2]))]),
        }
    } else {
        syn::Block { brace_token: syn::token::Brace::default(), stmts: vec![] }
    };
    let else_branch = if tokens.len() >= 3 {
        let else_expr = eval_lisp_arg(&tokens[2..3]);
        let else_body = match &else_expr {
            syn::Expr::If(_) => else_expr, // else-if chain
            _ => syn::Expr::Block(syn::ExprBlock {
                attrs: vec![], label: None,
                block: syn::Block {
                    brace_token: syn::token::Brace::default(),
                    stmts: vec![syn::Stmt::Expr(else_expr, None)],
                },
            }),
        };
        Some((syn::token::Else::default(), Box::new(else_body)))
    } else { None };

    syn::Expr::If(syn::ExprIf {
        attrs: vec![], if_token: syn::token::If::default(),
        cond: Box::new(cond), then_branch: then_block, else_branch,
    })
}

pub(crate) fn eval_cond(tokens: &[TokenTree]) -> syn::Expr {
    if tokens.is_empty() {
        return verbatim_expr(quote! {});
    }

    if let TokenTree::Group(g) = &tokens[0] {
        if g.delimiter() == Delimiter::Parenthesis {
            let inner: Vec<TokenTree> = g.stream().into_iter().collect();
            if !inner.is_empty() {
                // (else body...) — final else branch
                if is_ident(&inner[0], "else") {
                    let body: Vec<syn::Expr> = inner[1..].iter().map(|t| {
                        eval_lisp_arg(std::slice::from_ref(t))
                    }).collect();
                    return syn::Expr::Block(syn::ExprBlock {
                        attrs: vec![], label: None,
                        block: syn::Block {
                            brace_token: syn::token::Brace::default(),
                            stmts: body.into_iter().map(|e| syn::Stmt::Expr(e, None)).collect(),
                        },
                    });
                }

                // (cond body...) — regular condition branch
                let cond = eval_lisp_arg(&inner[0..1]);
                let body: Vec<LispOutput> = inner[1..].iter().map(|t| {
                    LispOutput::Expr(eval_lisp_arg(std::slice::from_ref(t)))
                }).collect();
                let then_block = syn::Block {
                    brace_token: syn::token::Brace::default(),
                    stmts: build_block_stmts(body),
                };

                let else_branch = if tokens.len() > 1 {
                    let rest = eval_cond(&tokens[1..]);
                    let else_body = match &rest {
                        syn::Expr::If(_) => rest,
                        _ => syn::Expr::Block(syn::ExprBlock {
                            attrs: vec![], label: None,
                            block: syn::Block {
                                brace_token: syn::token::Brace::default(),
                                stmts: vec![syn::Stmt::Expr(rest, None)],
                            },
                        }),
                    };
                    Some((syn::token::Else::default(), Box::new(else_body)))
                } else { None };

                return syn::Expr::If(syn::ExprIf {
                    attrs: vec![], if_token: syn::token::If::default(),
                    cond: Box::new(cond), then_branch: then_block, else_branch,
                });
            }
        }
    }
    verbatim_expr(quote! {})
}

/// Helper: build a `syn::Stmt::Local` (let binding).
fn make_local(pat: syn::Pat, init: Option<syn::Expr>) -> syn::Stmt {
    syn::Stmt::Local(syn::Local {
        attrs: vec![], let_token: syn::token::Let::default(), pat,
        init: init.map(|e| syn::LocalInit {
            eq_token: syn::token::Eq::default(), expr: Box::new(e), diverge: None,
        }),
        semi_token: syn::token::Semi::default(),
    })
}

pub(crate) fn eval_let(tokens: &[TokenTree]) -> LispOutput {
    if tokens.is_empty() { return LispOutput::Tokens(quote! {}); }

    // Scoped let: (let ((var init)...) body...)
    // First token is a group containing (var init) pairs, followed by body groups
    if let TokenTree::Group(g) = &tokens[0] {
        if g.delimiter() == Delimiter::Parenthesis {
            let outer_inner: Vec<TokenTree> = g.stream().into_iter().collect();
            // Check if this looks like scoped let: first element is a group
            let is_scoped = !outer_inner.is_empty() && matches!(&outer_inner[0], TokenTree::Group(g) if g.delimiter() == Delimiter::Parenthesis);
            if is_scoped {
                let mut bindings = Vec::new();
                for tt in &outer_inner {
                    if let TokenTree::Group(bg) = tt {
                        if bg.delimiter() == Delimiter::Parenthesis {
                            let bind_inner: Vec<TokenTree> = bg.stream().into_iter().collect();
                            if bind_inner.len() >= 2 {
                                let var = &bind_inner[0];
                                let val = eval_lisp_arg(&bind_inner[1..]);
                                bindings.push(quote! { let mut #var = #val; });
                            }
                        }
                    }
                }
                let body_items = parse_body_items(tokens, 1);
                return LispOutput::Tokens(quote! { { #(#bindings)* #(#body_items);* } });
            }
        }
    }

    // (let else (Pat = expr) (fallback))
    if is_ident(&tokens[0], "else") && tokens.len() >= 3 {
        if let TokenTree::Group(g) = &tokens[1] {
            if g.delimiter() == Delimiter::Parenthesis {
                let inner: Vec<TokenTree> = g.stream().into_iter().collect();
                // Find `=` separator
                if let Some(eq_pos) = inner.iter().position(|t| {
                    if let TokenTree::Punct(p) = t { p.as_char() == '=' } else { false }
                }) {
                    let pat: TokenStream2 = inner[..eq_pos].iter().cloned().collect();
                    let val_tokens = &inner[eq_pos + 1..];
                    let val = eval_lisp_arg(val_tokens);
                    if let Some(TokenTree::Group(fb)) = tokens.get(2) {
                        if fb.delimiter() == Delimiter::Parenthesis {
                            let fb_inner: Vec<TokenTree> = fb.stream().into_iter().collect();
                            let fallback = eval_lisp_expr(&fb_inner);
                            let span = tokens[0].span();
                            return LispOutput::Tokens(quote_spanned! { span => let #pat = #val else { #fallback; }; });
                        }
                    }
                }
            }
        }
    }

    // struct destructuring: (let Name { fields... } val)
    if tokens.len() >= 3 {
        if let TokenTree::Ident(_) = &tokens[0] {
            if !is_ident(&tokens[0], "mut") && !is_ident(&tokens[0], "else") {
                if let TokenTree::Group(g) = &tokens[1] {
                    if g.delimiter() == Delimiter::Brace {
                        let name = &tokens[0];
                        let fields = g.stream();
                        let val = eval_lisp_arg(&tokens[2..3]);
                        return LispOutput::Tokens(quote! { let #name { #fields } = #val; });
                    }
                }
            }
        }
    }

    // (let mut var val) or (let var val) — simple cases → syn::Stmt::Local
    let mut i = 0;
    let is_mut = is_ident(&tokens[i], "mut");
    if is_mut { i += 1; }

    if i >= tokens.len() { return LispOutput::Tokens(quote! {}); }

    // Check for typed let: (let (var Type) val)
    if let TokenTree::Group(g) = &tokens[i] {
        if g.delimiter() == Delimiter::Parenthesis {
            let inner: Vec<TokenTree> = g.stream().into_iter().collect();
            if inner.len() >= 2 {
                if let TokenTree::Ident(var_ident) = &inner[0] {
                    let raw_type: TokenStream2 = inner[1..].iter().cloned().collect();
                    if let Ok(ty) = syn::parse2::<syn::Type>(raw_type) {
                        let mut_token = if is_mut { Some(syn::token::Mut::default()) } else { None };
                        let pat = syn::Pat::Type(syn::PatType {
                            attrs: vec![],
                            pat: Box::new(syn::Pat::Ident(syn::PatIdent {
                                attrs: vec![], by_ref: None,
                                mutability: mut_token,
                                ident: var_ident.clone(),
                                subpat: None,
                            })),
                            colon_token: syn::token::Colon::default(),
                            ty: Box::new(ty),
                        });
                        i += 1;
                        let init = if i < tokens.len() {
                            Some(eval_lisp_arg(&tokens[i..]))
                        } else { None };
                        return LispOutput::Stmt(make_local(pat, init));
                    }
                }
            }
        }
    }

    // Simple untyped let: (let var val) or (let mut var val)
    if let TokenTree::Ident(var_ident) = &tokens[i] {
        let mut_token = if is_mut { Some(syn::token::Mut::default()) } else { None };
        let pat = syn::Pat::Ident(syn::PatIdent {
            attrs: vec![], by_ref: None,
            mutability: mut_token,
            ident: var_ident.clone(),
            subpat: None,
        });
        i += 1;
        let init = if i < tokens.len() {
            Some(eval_lisp_arg(&tokens[i..]))
        } else { None };
        return LispOutput::Stmt(make_local(pat, init));
    }

    // Fallback for non-ident patterns
    let var_name = &tokens[i];
    let span = var_name.span();
    i += 1;
    if i < tokens.len() {
        let val = eval_lisp_arg(&tokens[i..]);
        if is_mut {
            LispOutput::Tokens(quote_spanned! { span => let mut #var_name = #val; })
        } else {
            LispOutput::Tokens(quote_spanned! { span => let #var_name = #val; })
        }
    } else if is_mut {
        LispOutput::Tokens(quote_spanned! { span => let mut #var_name; })
    } else {
        LispOutput::Tokens(quote_spanned! { span => let #var_name; })
    }
}

/// Parse a token stream as a syn::Pat using Pat::parse_multi.
fn parse_pat(ts: TokenStream2) -> syn::Pat {
    struct PatWrapper(syn::Pat);
    impl syn::parse::Parse for PatWrapper {
        fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
            syn::Pat::parse_multi(input).map(PatWrapper)
        }
    }
    match syn::parse2::<PatWrapper>(ts.clone()) {
        Ok(PatWrapper(pat)) => pat,
        Err(_) => syn::Pat::Verbatim(ts),
    }
}

pub(crate) fn eval_for(tokens: &[TokenTree]) -> syn::Expr {
    // (for var in iter (body)...) or (for (pat) in iter (body)...)
    if tokens.len() < 3 { return verbatim_expr(quote! {}); }

    // Check if first token is a pattern group
    let (pat, in_idx) = if let TokenTree::Group(g) = &tokens[0] {
        if g.delimiter() == Delimiter::Parenthesis {
            let inner = g.stream();
            let pat_ts = quote! { (#inner) };
            (parse_pat(pat_ts), 1)
        } else {
            let v = &tokens[0];
            let v_ts = quote! { #v };
            (parse_pat(v_ts), 1)
        }
    } else {
        let v = &tokens[0];
        let v_ts = quote! { #v };
        (parse_pat(v_ts), 1)
    };

    // tokens[in_idx] should be `in`
    if in_idx + 1 >= tokens.len() { return verbatim_expr(quote! {}); }
    let iter_expr = eval_lisp_arg(&tokens[in_idx + 1..in_idx + 2]);
    let mut body = Vec::new();
    for tt in &tokens[in_idx + 2..] {
        if let TokenTree::Group(g) = tt {
            if g.delimiter() == Delimiter::Parenthesis {
                let inner: Vec<TokenTree> = g.stream().into_iter().collect();
                body.push(eval_lisp_expr(&inner));
            }
        }
    }
    syn::Expr::ForLoop(syn::ExprForLoop {
        attrs: vec![], label: None,
        for_token: syn::token::For::default(),
        pat: Box::new(pat),
        in_token: syn::token::In::default(),
        expr: Box::new(iter_expr),
        body: syn::Block {
            brace_token: syn::token::Brace::default(),
            stmts: build_block_stmts(body),
        },
    })
}

pub(crate) fn eval_while(tokens: &[TokenTree]) -> syn::Expr {
    if tokens.is_empty() { return verbatim_expr(quote! {}); }

    // Check for while let: (while let (Pat = cond) body...)
    // Keep while-let as Verbatim for now
    if is_ident(&tokens[0], "let") && tokens.len() >= 2 {
        if let TokenTree::Group(g) = &tokens[1] {
            if g.delimiter() == Delimiter::Parenthesis {
                let inner: Vec<TokenTree> = g.stream().into_iter().collect();
                if let Some(eq_pos) = inner.iter().position(|t| {
                    if let TokenTree::Punct(p) = t { p.as_char() == '=' } else { false }
                }) {
                    let pat: TokenStream2 = inner[..eq_pos].iter().cloned().collect();
                    let val = eval_lisp_arg(&inner[eq_pos + 1..]);
                    let mut body = Vec::new();
                    for tt in &tokens[2..] {
                        if let TokenTree::Group(g) = tt {
                            if g.delimiter() == Delimiter::Parenthesis {
                                let inner: Vec<TokenTree> = g.stream().into_iter().collect();
                                body.push(eval_lisp_expr(&inner));
                            }
                        }
                    }
                    return verbatim_expr(quote! { while let #pat = #val { #(#body);* } });
                }
            }
        }
    }

    let cond = eval_lisp_arg(&tokens[0..1]);
    let mut body = Vec::new();
    for tt in &tokens[1..] {
        if let TokenTree::Group(g) = tt {
            if g.delimiter() == Delimiter::Parenthesis {
                let inner: Vec<TokenTree> = g.stream().into_iter().collect();
                body.push(eval_lisp_expr(&inner));
            }
        }
    }
    syn::Expr::While(syn::ExprWhile {
        attrs: vec![], label: None,
        while_token: syn::token::While::default(),
        cond: Box::new(cond),
        body: syn::Block {
            brace_token: syn::token::Brace::default(),
            stmts: build_block_stmts(body),
        },
    })
}

pub(crate) fn find_fat_arrow(tokens: &[TokenTree]) -> Option<usize> {
    (0..tokens.len().saturating_sub(1))
        .find(|&i| is_punct(&tokens[i], '=') && is_punct(&tokens[i + 1], '>'))
}

/// Split match arm pattern into pattern and optional guard.
/// `n if (> n 0)` → pattern `n`, guard `Some(n > 0)`
fn split_pattern_guard_syn(tokens: &[TokenTree]) -> (syn::Pat, Option<syn::Expr>) {
    for (i, t) in tokens.iter().enumerate() {
        if is_ident(t, "if") {
            let pat_ts: TokenStream2 = tokens[..i].iter().cloned().collect();
            let guard = eval_lisp_arg(&tokens[i + 1..]);
            return (parse_pat(pat_ts), Some(guard));
        }
    }
    let pat_ts: TokenStream2 = tokens.iter().cloned().collect();
    (parse_pat(pat_ts), None)
}

pub(crate) fn eval_match(tokens: &[TokenTree]) -> syn::Expr {
    if tokens.is_empty() { return verbatim_expr(quote! {}); }
    let scrutinee = eval_lisp_arg(&tokens[0..1]);
    let mut arms = Vec::new();

    for tt in &tokens[1..] {
        if let TokenTree::Group(g) = tt {
            if g.delimiter() == Delimiter::Parenthesis {
                let inner: Vec<TokenTree> = g.stream().into_iter().collect();
                if let Some(arrow_pos) = find_fat_arrow(&inner) {
                    let pat_tokens = &inner[..arrow_pos];
                    let body = &inner[arrow_pos + 2..]; // skip = and >

                    // Split pattern and optional guard
                    let (pat, guard) = split_pattern_guard_syn(pat_tokens);

                    // Match body evaluation: mirrors old lisp_match_arg! behavior.
                    let body_expr = if body.len() == 1 {
                        if let TokenTree::Group(bg) = &body[0] {
                            if bg.delimiter() == Delimiter::Parenthesis {
                                let body_inner: Vec<TokenTree> = bg.stream().into_iter().collect();
                                eval_match_body_expr(&body_inner)
                            } else {
                                eval_lisp_arg(body)
                            }
                        } else {
                            eval_lisp_arg(std::slice::from_ref(&body[0]))
                        }
                    } else if body.is_empty() {
                        verbatim_expr(quote! { {} })
                    } else {
                        let items: Vec<syn::Expr> = body.iter()
                            .map(|t| eval_lisp_arg(std::slice::from_ref(t)))
                            .collect();
                        verbatim_expr(quote! { { #(#items);* } })
                    };

                    arms.push(syn::Arm {
                        attrs: vec![],
                        pat,
                        guard: guard.map(|g| (syn::token::If::default(), Box::new(g))),
                        fat_arrow_token: syn::token::FatArrow::default(),
                        body: Box::new(body_expr),
                        comma: Some(syn::token::Comma::default()),
                    });
                }
            }
        }
    }

    syn::Expr::Match(syn::ExprMatch {
        attrs: vec![], match_token: syn::token::Match::default(),
        expr: Box::new(scrutinee), brace_token: syn::token::Brace::default(), arms,
    })
}

/// Evaluate match arm body contents as a syn::Expr.
fn eval_match_body_expr(tokens: &[TokenTree]) -> syn::Expr {
    if tokens.len() == 1 {
        let t = &tokens[0];
        if let TokenTree::Group(g) = t {
            if g.delimiter() == Delimiter::Parenthesis {
                return eval_lisp_expr(&g.stream().into_iter().collect::<Vec<_>>()).into_expr();
            }
        }
        return eval_lisp_arg(std::slice::from_ref(t));
    }
    eval_lisp_expr(tokens).into_expr()
}

// ─── Item-level forms: const, static, type ───────────────────────────────────
// Handles: [vis] const NAME TYPE [=] VALUE
//          [vis] static [mut] NAME TYPE [=] VALUE
//          [vis] type NAME = TARGET
// Produces complete items including trailing semicolons.
pub(crate) fn eval_item_form(tokens: &[TokenTree]) -> Option<LispOutput> {
    if tokens.is_empty() { return None; }

    // Parse optional visibility prefix
    let (vis_ts, vis_consumed) = parse_visibility(tokens).ok()?;
    let rest = &tokens[vis_consumed..];
    if rest.is_empty() { return None; }

    // Determine the keyword: const, static, or type
    if is_ident(&rest[0], "const") {
        // Skip "const fn" — that's a function, not a const variable
        if rest.len() >= 2 && is_ident(&rest[1], "fn") {
            return None;
        }
        return eval_const_static(rest, &vis_ts, false, false).ok();
    }

    if is_ident(&rest[0], "static") {
        let is_mut = rest.len() >= 2 && is_ident(&rest[1], "mut");
        return eval_const_static(rest, &vis_ts, true, is_mut).ok();
    }

    if is_ident(&rest[0], "type") {
        return eval_type_alias(rest, &vis_ts).ok();
    }

    None
}

/// Parse: const/static [mut] NAME TYPE [=] VALUE
/// `keyword_rest` starts at "const" or "static".
pub(crate) fn eval_const_static(keyword_rest: &[TokenTree], vis: &TokenStream2, is_static: bool, is_mut: bool) -> syn::Result<LispOutput> {
    let mut i = 1; // skip "const" or "static"
    if is_mut { i += 1; } // skip "mut"

    if i >= keyword_rest.len() {
        return Err(syn::Error::new(keyword_rest[0].span(), "expected name after const/static keyword"));
    }

    let name = &keyword_rest[i];
    i += 1;

    if i >= keyword_rest.len() {
        return Err(syn::Error::new(name.span(), "expected type after name in const/static"));
    }

    // Look for `=` to separate type from value
    let eq_pos = keyword_rest[i..].iter().position(|t| is_punct(t, '='));

    let (type_ts, value_ts) = if let Some(pos) = eq_pos {
        // [vis] const/static NAME TYPE = VALUE
        let type_tokens: TokenStream2 = keyword_rest[i..i + pos].iter().cloned().collect();
        let value_tokens = &keyword_rest[i + pos + 1..];
        let val = eval_lisp_arg(value_tokens);
        (validate_type(type_tokens), val)
    } else {
        // [vis] const/static NAME TYPE_TT VALUE_TT (no = separator)
        let type_tt = &keyword_rest[i];
        i += 1;
        let type_ts = if let TokenTree::Group(g) = type_tt {
            if g.delimiter() == Delimiter::Parenthesis {
                // Parenthesized type: parse through validate_type
                validate_type(g.stream())
            } else {
                validate_type(std::iter::once(type_tt.clone()).collect())
            }
        } else {
            validate_type(std::iter::once(type_tt.clone()).collect())
        };
        if i >= keyword_rest.len() {
            return Err(syn::Error::new(keyword_rest[0].span(), "expected value in const/static"));
        }
        let val = eval_lisp_arg(&keyword_rest[i..]);
        (type_ts, val)
    };

    let item_kw = if is_static {
        if is_mut {
            quote! { static mut }
        } else {
            quote! { static }
        }
    } else {
        quote! { const }
    };

    Ok(LispOutput::Tokens(quote! { #vis #item_kw #name : #type_ts = #value_ts; }))
}

/// Parse: type NAME = TARGET
/// `keyword_rest` starts at "type".
pub(crate) fn eval_type_alias(keyword_rest: &[TokenTree], vis: &TokenStream2) -> syn::Result<LispOutput> {
    // type NAME = TARGET
    if keyword_rest.len() < 4 {
        return Err(syn::Error::new(keyword_rest[0].span(), "expected: type NAME = TARGET"));
    }

    let name = &keyword_rest[1]; // NAME
    // keyword_rest[2] should be `=`
    if !is_punct(&keyword_rest[2], '=') {
        return Err(syn::Error::new(keyword_rest[2].span(), "expected `=` in type alias"));
    }
    let target: TokenStream2 = keyword_rest[3..].iter().cloned().collect();
    let target_validated = validate_type(target);

    Ok(LispOutput::Tokens(quote! { #vis type #name = #target_validated; }))
}

pub(crate) fn eval_closure(tokens: &[TokenTree]) -> syn::Expr {
    if tokens.is_empty() { return verbatim_expr(quote! { || {} }); }

    let mut i = 0;
    let is_move = i < tokens.len() && is_ident(&tokens[i], "move");
    if is_move { i += 1; }

    // Parse parameter list (a paren group containing (name Type) pairs)
    let mut inputs = syn::punctuated::Punctuated::new();
    if i < tokens.len() {
        if let TokenTree::Group(g) = &tokens[i] {
            if g.delimiter() == Delimiter::Parenthesis {
                let param_tokens: Vec<TokenTree> = g.stream().into_iter().collect();
                for tt in &param_tokens {
                    if let TokenTree::Group(pg) = tt {
                        if pg.delimiter() == Delimiter::Parenthesis {
                            let inner: Vec<TokenTree> = pg.stream().into_iter().collect();
                            if inner.len() >= 2 {
                                let name = &inner[0];
                                let raw_type: TokenStream2 = inner[1..].iter().cloned().collect();
                                let typ = validate_type(raw_type);
                                let pat_ts = quote! { #name: #typ };
                                inputs.push(parse_pat(pat_ts));
                            } else if inner.len() == 1 {
                                let name = &inner[0];
                                let pat_ts = quote! { #name };
                                inputs.push(parse_pat(pat_ts));
                            }
                        }
                    }
                }
                i += 1;
            }
        }
    }

    // Check for return type: -> Type
    let mut output = syn::ReturnType::Default;
    if i + 1 < tokens.len() {
        if let TokenTree::Punct(p1) = &tokens[i] {
            if p1.as_char() == '-' {
                if let TokenTree::Punct(p2) = &tokens[i + 1] {
                    if p2.as_char() == '>' {
                        i += 2;
                        let mut rt_tokens = Vec::new();
                        while i < tokens.len() {
                            if let TokenTree::Group(g) = &tokens[i] {
                                if g.delimiter() == Delimiter::Parenthesis {
                                    break;
                                }
                            }
                            rt_tokens.push(tokens[i].clone());
                            i += 1;
                        }
                        if !rt_tokens.is_empty() {
                            let rt: TokenStream2 = rt_tokens.into_iter().collect();
                            let validated = validate_type(rt);
                            if let Ok(ty) = syn::parse2::<syn::Type>(validated.clone()) {
                                output = syn::ReturnType::Type(syn::token::RArrow::default(), Box::new(ty));
                            } else {
                                output = syn::ReturnType::Type(
                                    syn::token::RArrow::default(),
                                    Box::new(syn::Type::Verbatim(validated)),
                                );
                            }
                        }
                    }
                }
            }
        }
    }

    // Parse body items
    let body_items = parse_body_items(tokens, i);
    let body_block = syn::Expr::Block(syn::ExprBlock {
        attrs: vec![], label: None,
        block: syn::Block {
            brace_token: syn::token::Brace::default(),
            stmts: build_block_stmts(body_items),
        },
    });

    syn::Expr::Closure(syn::ExprClosure {
        attrs: vec![], lifetimes: None, constness: None, movability: None, asyncness: None,
        capture: if is_move { Some(syn::token::Move::default()) } else { None },
        or1_token: syn::token::Or::default(), inputs, or2_token: syn::token::Or::default(),
        output, body: Box::new(body_block),
    })
}
