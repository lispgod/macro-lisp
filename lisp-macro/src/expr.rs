use proc_macro2::{Delimiter, Punct, Spacing, TokenStream as TokenStream2, TokenTree};
use quote::quote;

use crate::forms::{
    eval_closure, eval_cond, eval_for, eval_if, eval_item_form, eval_let, eval_match, eval_while,
};
use crate::helpers::{consume_type_path, is_ident, is_punct, validate_type};
use crate::output::{build_block_stmts, paren_wrap, LispOutput};
use crate::shared::parse_body_items;

// ─── syn AST construction helpers ────────────────────────────────────────────

/// Build a function call expression: `func(arg1, arg2, ...)`
fn make_call(func: syn::Expr, args: Vec<syn::Expr>) -> syn::Expr {
    let mut punct_args = syn::punctuated::Punctuated::new();
    for arg in args {
        punct_args.push(arg);
    }
    syn::Expr::Call(syn::ExprCall {
        attrs: vec![],
        func: Box::new(func),
        paren_token: syn::token::Paren::default(),
        args: punct_args,
    })
}

/// Build a method call expression: `receiver.method(arg1, arg2, ...)`
fn make_method_call(
    receiver: syn::Expr,
    method: proc_macro2::Ident,
    args: Vec<syn::Expr>,
) -> syn::Expr {
    let mut punct_args = syn::punctuated::Punctuated::new();
    for arg in args {
        punct_args.push(arg);
    }
    syn::Expr::MethodCall(syn::ExprMethodCall {
        attrs: vec![],
        receiver: Box::new(receiver),
        dot_token: syn::token::Dot::default(),
        method,
        turbofish: None,
        paren_token: syn::token::Paren::default(),
        args: punct_args,
    })
}

/// Build a field access chain: `obj.field1.field2...`
fn make_field_access(base: syn::Expr, field: &proc_macro2::Ident) -> syn::Expr {
    syn::Expr::Field(syn::ExprField {
        attrs: vec![],
        base: Box::new(base),
        dot_token: syn::token::Dot::default(),
        member: syn::Member::Named(field.clone()),
    })
}

/// Build a macro invocation expression: `name!(arg1, arg2, ...)`
fn make_macro_call(path: syn::Path, args: Vec<syn::Expr>) -> syn::Expr {
    let args_ts: TokenStream2 = quote! { #(#args),* };
    syn::Expr::Macro(syn::ExprMacro {
        attrs: vec![],
        mac: syn::Macro {
            path,
            bang_token: syn::token::Not::default(),
            delimiter: syn::MacroDelimiter::Paren(syn::token::Paren::default()),
            tokens: args_ts,
        },
    })
}

// ─── Operator dispatch helpers for eval_lisp_expr ────────────────────────────
// These eliminate the repetitive per-operator match arms in the Punct branch.

/// Evaluate a binary operation: `(op a b)` → `a op b`
pub(crate) fn eval_binary_op(operands: &[TokenTree], op: syn::BinOp) -> syn::Expr {
    let left = paren_wrap(eval_lisp_arg(&operands[0..1]));
    let right = paren_wrap(eval_lisp_arg(&operands[1..2]));
    syn::Expr::Binary(syn::ExprBinary {
        attrs: vec![],
        left: Box::new(left),
        op,
        right: Box::new(right),
    })
}

/// Evaluate a variadic arithmetic operation: `(op a b c ...)` → `a op b op c op ...`
pub(crate) fn eval_variadic_op(operands: &[TokenTree], op: syn::BinOp) -> syn::Expr {
    let left = paren_wrap(eval_lisp_arg(&operands[0..1]));
    let right = paren_wrap(eval_lisp_arg(&operands[1..2]));
    let mut result = syn::Expr::Binary(syn::ExprBinary {
        attrs: vec![],
        left: Box::new(left),
        op,
        right: Box::new(right),
    });
    for t in &operands[2..] {
        let c = paren_wrap(eval_lisp_arg(std::slice::from_ref(t)));
        result = syn::Expr::Binary(syn::ExprBinary {
            attrs: vec![],
            left: Box::new(result),
            op,
            right: Box::new(c),
        });
    }
    result
}

/// Evaluate a compound assignment: `(op= lhs... rhs)` → `lhs op= rhs;`
pub(crate) fn eval_compound_assign(rest: &[TokenTree], op_char: char) -> TokenStream2 {
    let lhs: TokenStream2 = rest[..rest.len() - 1].iter().cloned().collect();
    let rhs = eval_lisp_arg(&rest[rest.len() - 1..]);
    let op = Punct::new(op_char, Spacing::Joint);
    quote! { #lhs #op = #rhs; }
}

/// Dispatch punctuation-based expressions in eval_lisp_expr.
/// Returns Some(result) if handled, None otherwise.
pub(crate) fn eval_punct_expr(tokens: &[TokenTree]) -> Option<LispOutput> {
    let TokenTree::Punct(p) = &tokens[0] else {
        return None;
    };
    let ch = p.as_char();

    // ── Two-character operators (check second punct token first) ──
    if tokens.len() >= 2 {
        if let TokenTree::Punct(p2) = &tokens[1] {
            let ch2 = p2.as_char();
            match (ch, ch2) {
                // Range operators: (.. a b) → a..b, (..= a b) → a..=b, (.. a) → a.., (..) → ..
                ('.', '.') => {
                    // Check for ..= (three punct tokens)
                    if tokens.len() >= 3 {
                        if let TokenTree::Punct(p3) = &tokens[2] {
                            if p3.as_char() == '=' {
                                // (..= a b) → a..=b
                                if tokens.len() >= 5 {
                                    let a = eval_lisp_arg(&tokens[3..4]);
                                    let b = eval_lisp_arg(&tokens[4..5]);
                                    return Some(LispOutput::Expr(syn::Expr::Range(
                                        syn::ExprRange {
                                            attrs: vec![],
                                            start: Some(Box::new(a)),
                                            limits: syn::RangeLimits::Closed(
                                                syn::token::DotDotEq::default(),
                                            ),
                                            end: Some(Box::new(b)),
                                        },
                                    )));
                                }
                            }
                        }
                    }
                    // (.. a b) → a..b
                    if tokens.len() >= 4 {
                        let a = eval_lisp_arg(&tokens[2..3]);
                        let b = eval_lisp_arg(&tokens[3..4]);
                        return Some(LispOutput::Expr(syn::Expr::Range(syn::ExprRange {
                            attrs: vec![],
                            start: Some(Box::new(a)),
                            limits: syn::RangeLimits::HalfOpen(syn::token::DotDot::default()),
                            end: Some(Box::new(b)),
                        })));
                    }
                    // (.. a) → a..
                    if tokens.len() == 3 {
                        let a = eval_lisp_arg(&tokens[2..3]);
                        return Some(LispOutput::Expr(syn::Expr::Range(syn::ExprRange {
                            attrs: vec![],
                            start: Some(Box::new(a)),
                            limits: syn::RangeLimits::HalfOpen(syn::token::DotDot::default()),
                            end: None,
                        })));
                    }
                    // (..) → ..
                    if tokens.len() == 2 {
                        return Some(LispOutput::Expr(syn::Expr::Range(syn::ExprRange {
                            attrs: vec![],
                            start: None,
                            limits: syn::RangeLimits::HalfOpen(syn::token::DotDot::default()),
                            end: None,
                        })));
                    }
                }
                // Comparison operators: operands start at index 2
                ('=', '=') if tokens.len() >= 4 => {
                    return Some(LispOutput::Expr(eval_binary_op(
                        &tokens[2..],
                        syn::BinOp::Eq(syn::token::EqEq::default()),
                    )))
                }
                ('!', '=') if tokens.len() >= 4 => {
                    return Some(LispOutput::Expr(eval_binary_op(
                        &tokens[2..],
                        syn::BinOp::Ne(syn::token::Ne::default()),
                    )))
                }
                ('>', '=') if tokens.len() >= 4 => {
                    return Some(LispOutput::Expr(eval_binary_op(
                        &tokens[2..],
                        syn::BinOp::Ge(syn::token::Ge::default()),
                    )))
                }
                ('<', '=') if tokens.len() >= 4 => {
                    return Some(LispOutput::Expr(eval_binary_op(
                        &tokens[2..],
                        syn::BinOp::Le(syn::token::Le::default()),
                    )))
                }
                // Logical operators: operands start at index 2
                ('&', '&') if tokens.len() >= 4 => {
                    return Some(LispOutput::Expr(eval_binary_op(
                        &tokens[2..],
                        syn::BinOp::And(syn::token::AndAnd::default()),
                    )))
                }
                ('|', '|') if tokens.len() >= 4 => {
                    return Some(LispOutput::Expr(eval_binary_op(
                        &tokens[2..],
                        syn::BinOp::Or(syn::token::OrOr::default()),
                    )))
                }
                // Compound assignment operators: (op= lhs... rhs)
                ('+', '=')
                | ('-', '=')
                | ('*', '=')
                | ('/', '=')
                | ('%', '=')
                | ('&', '=')
                | ('|', '=')
                | ('^', '=')
                    if tokens.len() >= 4 =>
                {
                    return Some(LispOutput::Tokens(eval_compound_assign(&tokens[2..], ch)));
                }
                // Shift operators: (<< a b) → a << b, (>> a b) → a >> b
                ('<', '<') if tokens.len() >= 4 => {
                    // Check for <<= (shift-left-assign): third token is '='
                    if tokens.len() >= 5 && is_punct(&tokens[2], '=') {
                        let rest = &tokens[3..];
                        let lhs: TokenStream2 = rest[..rest.len() - 1].iter().cloned().collect();
                        let rhs = eval_lisp_arg(&rest[rest.len() - 1..]);
                        return Some(LispOutput::Tokens(quote! { #lhs <<= #rhs; }));
                    }
                    return Some(LispOutput::Expr(eval_binary_op(
                        &tokens[2..],
                        syn::BinOp::Shl(syn::token::Shl::default()),
                    )));
                }
                ('>', '>') if tokens.len() >= 4 => {
                    // Check for >>= (shift-right-assign): third token is '='
                    if tokens.len() >= 5 && is_punct(&tokens[2], '=') {
                        let rest = &tokens[3..];
                        let lhs: TokenStream2 = rest[..rest.len() - 1].iter().cloned().collect();
                        let rhs = eval_lisp_arg(&rest[rest.len() - 1..]);
                        return Some(LispOutput::Tokens(quote! { #lhs >>= #rhs; }));
                    }
                    return Some(LispOutput::Expr(eval_binary_op(
                        &tokens[2..],
                        syn::BinOp::Shr(syn::token::Shr::default()),
                    )));
                }
                _ => {}
            }
        }
    }

    // ── Single-character operators ──
    match ch {
        // Variadic arithmetic: (+ a b ...), (- a b ...), (* a b ...), (/ a b ...), (% a b)
        '+' if tokens.len() >= 3 => {
            return Some(LispOutput::Expr(eval_variadic_op(
                &tokens[1..],
                syn::BinOp::Add(syn::token::Plus::default()),
            )))
        }
        '-' if tokens.len() >= 3 => {
            return Some(LispOutput::Expr(eval_variadic_op(
                &tokens[1..],
                syn::BinOp::Sub(syn::token::Minus::default()),
            )))
        }
        '*' if tokens.len() >= 3 => {
            return Some(LispOutput::Expr(eval_variadic_op(
                &tokens[1..],
                syn::BinOp::Mul(syn::token::Star::default()),
            )))
        }
        '/' if tokens.len() >= 3 => {
            return Some(LispOutput::Expr(eval_variadic_op(
                &tokens[1..],
                syn::BinOp::Div(syn::token::Slash::default()),
            )))
        }
        '&' if tokens.len() >= 3 => {
            return Some(LispOutput::Expr(eval_variadic_op(
                &tokens[1..],
                syn::BinOp::BitAnd(syn::token::And::default()),
            )))
        }
        '|' if tokens.len() >= 3 => {
            return Some(LispOutput::Expr(eval_variadic_op(
                &tokens[1..],
                syn::BinOp::BitOr(syn::token::Or::default()),
            )))
        }
        '^' if tokens.len() >= 3 => {
            return Some(LispOutput::Expr(eval_variadic_op(
                &tokens[1..],
                syn::BinOp::BitXor(syn::token::Caret::default()),
            )))
        }
        '%' if tokens.len() == 3 => {
            return Some(LispOutput::Expr(eval_binary_op(
                &tokens[1..],
                syn::BinOp::Rem(syn::token::Percent::default()),
            )));
        }
        // Simple comparison: (> a b), (< a b)
        '>' if tokens.len() == 3 => {
            return Some(LispOutput::Expr(eval_binary_op(
                &tokens[1..],
                syn::BinOp::Gt(syn::token::Gt::default()),
            )));
        }
        '<' if tokens.len() == 3 => {
            return Some(LispOutput::Expr(eval_binary_op(
                &tokens[1..],
                syn::BinOp::Lt(syn::token::Lt::default()),
            )));
        }
        // Simple assignment: (= var rhs...) or (= complex.lhs rhs)
        '=' if tokens.len() >= 3 => {
            // If the first token after `=` is a simple ident NOT followed by `.`, `::`, or `[`,
            // treat it as the LHS variable and everything after as the RHS expression.
            // This correctly handles (= num Some(i + 1)) → num = Some(i + 1);
            if let TokenTree::Ident(_) = &tokens[1] {
                let is_complex_lhs = tokens.len() > 2
                    && (is_punct(&tokens[2], '.')
                        || is_punct(&tokens[2], ':')
                        || matches!(&tokens[2], TokenTree::Group(g) if g.delimiter() == Delimiter::Bracket));
                if !is_complex_lhs {
                    let lhs = eval_lisp_arg(&tokens[1..2]);
                    let rhs = eval_lisp_arg(&tokens[2..]);
                    let assign = syn::Expr::Assign(syn::ExprAssign {
                        attrs: vec![],
                        left: Box::new(lhs),
                        eq_token: syn::token::Eq::default(),
                        right: Box::new(rhs),
                    });
                    return Some(LispOutput::Tokens(quote! { #assign; }));
                }
            }
            // Complex LHS (e.g., self.x, v[0]): last token is RHS, everything else is LHS
            let lhs: TokenStream2 = tokens[1..tokens.len() - 1].iter().cloned().collect();
            let rhs = eval_lisp_arg(&tokens[tokens.len() - 1..]);
            let lhs_expr =
                syn::parse2::<syn::Expr>(lhs.clone()).unwrap_or(syn::Expr::Verbatim(lhs));
            let assign = syn::Expr::Assign(syn::ExprAssign {
                attrs: vec![],
                left: Box::new(lhs_expr),
                eq_token: syn::token::Eq::default(),
                right: Box::new(rhs),
            });
            return Some(LispOutput::Tokens(quote! { #assign; }));
        }
        // Unary not: (! x)
        '!' if tokens.len() == 2 => {
            let e = eval_lisp_arg(&tokens[1..2]);
            return Some(LispOutput::Expr(syn::Expr::Unary(syn::ExprUnary {
                attrs: vec![],
                op: syn::UnOp::Not(syn::token::Not::default()),
                expr: Box::new(paren_wrap(e)),
            })));
        }
        // Field access and method calls: (. obj field1 field2 ...)
        // Method call on expression: (. obj (method arg1 arg2))
        '.' if tokens.len() >= 3 => {
            let obj = eval_lisp_arg(&tokens[1..2]);
            let result = tokens[2..].iter().fold(obj, |acc, f| {
                match f {
                    TokenTree::Ident(ident) => make_field_access(acc, ident),
                    TokenTree::Literal(lit) => {
                        // Tuple field access: obj.0, obj.1, etc.
                        if let Ok(idx) = lit.to_string().parse::<u32>() {
                            syn::Expr::Field(syn::ExprField {
                                attrs: vec![],
                                base: Box::new(acc),
                                dot_token: syn::token::Dot::default(),
                                member: syn::Member::Unnamed(syn::Index {
                                    index: idx,
                                    span: lit.span(),
                                }),
                            })
                        } else {
                            acc
                        }
                    }
                    TokenTree::Group(g) if g.delimiter() == Delimiter::Parenthesis => {
                        // Method call: (. obj (method arg1 arg2))
                        let inner: Vec<TokenTree> = g.stream().into_iter().collect();
                        if !inner.is_empty() {
                            if let TokenTree::Ident(method) = &inner[0] {
                                let args: Vec<syn::Expr> = inner[1..]
                                    .iter()
                                    .map(|t| eval_lisp_arg(std::slice::from_ref(t)))
                                    .collect();
                                make_method_call(acc, method.clone(), args)
                            } else {
                                acc
                            }
                        } else {
                            acc
                        }
                    }
                    _ => acc,
                }
            });
            return Some(LispOutput::Expr(result));
        }
        // Try operator: (? x)
        '?' if tokens.len() == 2 => {
            let e = eval_lisp_arg(&tokens[1..2]);
            return Some(LispOutput::Expr(syn::Expr::Try(syn::ExprTry {
                attrs: vec![],
                expr: Box::new(e),
                question_token: syn::token::Question::default(),
            })));
        }
        _ => {}
    }

    None
}

/// Evaluate a lisp expression (the contents of a parenthesized group) into Rust code.
/// Evaluate a lisp S-expression token sequence into Rust code.
///
/// This is the **single source of truth** for all S-expression → Rust
/// transformation. It is called by `lisp_eval!`, by `eval_lisp_arg` for
/// parenthesized sub-expressions, and by item evaluators (`lisp_fn!`,
/// `lisp_impl!`, etc.) for body expressions.
///
/// **S-expression semantic for identifiers**: A single non-keyword identifier
/// is treated as a zero-argument function call (`f` → `f()`), matching the
/// S-expression convention where `(f)` means "call f". To reference a variable
/// value, use `(val x)` or pass it directly without lisp wrapping.
pub(crate) fn eval_lisp_expr(tokens: &[TokenTree]) -> LispOutput {
    if tokens.is_empty() {
        return LispOutput::Tokens(quote! {});
    }

    // Check for labeled forms: ('label loop/while/for/block ...)
    if tokens.len() >= 3 {
        if let TokenTree::Punct(p) = &tokens[0] {
            if p.as_char() == '\'' {
                if let TokenTree::Ident(label_ident) = &tokens[1] {
                    let syn_label = syn::Label {
                        name: syn::Lifetime {
                            apostrophe: p.span(),
                            ident: label_ident.clone(),
                        },
                        colon_token: syn::token::Colon::default(),
                    };
                    if tokens.len() > 2 {
                        if let TokenTree::Ident(kw) = &tokens[2] {
                            match kw.to_string().as_str() {
                                "loop" => {
                                    let body_items = parse_body_items(tokens, 3);
                                    return LispOutput::Expr(syn::Expr::Loop(syn::ExprLoop {
                                        attrs: vec![],
                                        label: Some(syn_label),
                                        loop_token: syn::token::Loop::default(),
                                        body: syn::Block {
                                            brace_token: syn::token::Brace::default(),
                                            stmts: build_block_stmts(body_items),
                                        },
                                    }));
                                }
                                "while" => {
                                    let inner_result = eval_while(&tokens[3..]);
                                    if let syn::Expr::While(mut w) = inner_result {
                                        w.label = Some(syn_label);
                                        return LispOutput::Expr(syn::Expr::While(w));
                                    }
                                    // Fallback (while-let returns Verbatim)
                                    let tick = &tokens[0];
                                    let label = &tokens[1];
                                    return LispOutput::Tokens(
                                        quote! { #tick #label : #inner_result },
                                    );
                                }
                                "for" => {
                                    let inner_result = eval_for(&tokens[3..]);
                                    if let syn::Expr::ForLoop(mut f) = inner_result {
                                        f.label = Some(syn_label);
                                        return LispOutput::Expr(syn::Expr::ForLoop(f));
                                    }
                                    let tick = &tokens[0];
                                    let label = &tokens[1];
                                    return LispOutput::Tokens(
                                        quote! { #tick #label : #inner_result },
                                    );
                                }
                                _ => {}
                            }
                        }
                    }
                }
            }
        }
    }

    // Check for item-level forms: [vis] const/static/type ...
    // These are dispatched here so they benefit from syn::Type validation.
    if let Some(result) = eval_item_form(tokens) {
        return result;
    }

    // Single token — return as-is (unless it's a keyword that needs special handling)
    if tokens.len() == 1 {
        let t = &tokens[0];
        // If it's a parenthesized group, recurse
        if let TokenTree::Group(g) = t {
            if g.delimiter() == Delimiter::Parenthesis {
                return eval_lisp_expr(&g.stream().into_iter().collect::<Vec<_>>());
            }
        }
        // Keywords that produce output even with zero arguments fall through
        if let TokenTree::Ident(id) = t {
            match id.to_string().as_str() {
                "vec" | "tuple" | "array" | "block" | "loop" | "break" | "continue" | "return"
                | "true" | "false" => { /* fall through to keyword handling below */ }
                // Single non-keyword ident → zero-arg function call (S-expression semantic:
                // (f) means "call f", matching old lisp! macro_rules! catch-all behavior)
                _ => return LispOutput::Tokens(quote! { #t() }),
            }
        } else {
            return LispOutput::Tokens(quote! { #t });
        }
    }

    // Check first token for special forms
    match &tokens[0] {
        TokenTree::Ident(id) => {
            let name = id.to_string();
            match name.as_str() {
                "new" => {
                    // (new Name (field val) ...) or (new Name (field val) (.. base))
                    // Also handle tuple struct: (new Name val1 val2) → Name(val1, val2)
                    // Also handle path-qualified: (new Name::Variant (field val) ...)
                    if tokens.len() >= 2 {
                        let (path_tokens, remaining) = consume_type_path(&tokens[1..]);
                        let struct_name_ts: TokenStream2 = path_tokens.into_iter().collect();
                        let field_start = tokens.len() - remaining.len();

                        let mut fields = Vec::new();
                        let mut spread = None;
                        let mut has_bare_args = false;

                        for tt in &tokens[field_start..] {
                            if let TokenTree::Group(g) = tt {
                                if g.delimiter() == Delimiter::Parenthesis {
                                    let inner: Vec<TokenTree> = g.stream().into_iter().collect();
                                    // Check for (.. base) spread syntax
                                    if inner.len() >= 2
                                        && is_punct(&inner[0], '.')
                                        && is_punct(&inner[1], '.')
                                    {
                                        let base = eval_lisp_arg(&inner[2..]);
                                        spread = Some(quote! { ..#base });
                                        continue;
                                    }
                                    if inner.len() >= 2 {
                                        let fname = &inner[0];
                                        let fval = eval_lisp_arg(&inner[1..]);
                                        fields.push(quote! { #fname: #fval });
                                    }
                                }
                            } else {
                                has_bare_args = true;
                            }
                        }

                        if has_bare_args && fields.is_empty() {
                            // Check if all bare args are identifiers → shorthand field init
                            let all_idents = tokens[field_start..]
                                .iter()
                                .all(|t| matches!(t, TokenTree::Ident(_)));
                            if all_idents {
                                let field_names: Vec<&TokenTree> =
                                    tokens[field_start..].iter().collect();
                                return LispOutput::Tokens(
                                    quote! { #struct_name_ts { #(#field_names),* } },
                                );
                            }
                            // Otherwise → tuple struct construction
                            let args: Vec<syn::Expr> = tokens[field_start..]
                                .iter()
                                .map(|t| eval_lisp_arg(std::slice::from_ref(t)))
                                .collect();
                            return LispOutput::Tokens(quote! { #struct_name_ts(#(#args),*) });
                        }

                        if let Some(spread_ts) = spread {
                            return LispOutput::Tokens(
                                quote! { #struct_name_ts { #(#fields,)* #spread_ts } },
                            );
                        }
                        return LispOutput::Tokens(quote! { #struct_name_ts { #(#fields),* } });
                    }
                }
                "r#struct" | "struct" => {
                    // (struct - lit Name (field val) ...)
                    if tokens.len() >= 4 && is_punct(&tokens[1], '-') {
                        if let TokenTree::Ident(lit_id) = &tokens[2] {
                            if *lit_id == "lit" {
                                let struct_name = &tokens[3];
                                let mut fields = Vec::new();
                                for tt in &tokens[4..] {
                                    if let TokenTree::Group(g) = tt {
                                        if g.delimiter() == Delimiter::Parenthesis {
                                            let inner: Vec<TokenTree> =
                                                g.stream().into_iter().collect();
                                            if inner.len() >= 2 {
                                                let fname = &inner[0];
                                                let fval = eval_lisp_arg(&inner[1..]);
                                                fields.push(quote! { #fname: #fval });
                                            }
                                        }
                                    }
                                }
                                return LispOutput::Tokens(
                                    quote! { #struct_name { #(#fields),* } },
                                );
                            }
                        }
                    }
                }
                "macro" => {
                    // (macro! name args...) — but first token is `macro`, second is `!`
                    if tokens.len() >= 3 && is_punct(&tokens[1], '!') {
                        let macro_name_tokens = &tokens[2..];
                        // Find the macro name (could be path like write)
                        let mut mi = 0;
                        let mut name_parts = Vec::new();
                        while mi < macro_name_tokens.len() {
                            if let TokenTree::Ident(_) = &macro_name_tokens[mi] {
                                name_parts.push(macro_name_tokens[mi].clone());
                                mi += 1;
                                if mi + 1 < macro_name_tokens.len()
                                    && is_punct(&macro_name_tokens[mi], ':')
                                    && is_punct(&macro_name_tokens[mi + 1], ':')
                                {
                                    name_parts.push(macro_name_tokens[mi].clone());
                                    name_parts.push(macro_name_tokens[mi + 1].clone());
                                    mi += 2;
                                    continue;
                                }
                                break;
                            }
                            break;
                        }
                        let name_ts: TokenStream2 = name_parts.into_iter().collect();
                        let args: Vec<syn::Expr> = macro_name_tokens[mi..]
                            .iter()
                            .map(|t| eval_lisp_arg(std::slice::from_ref(t)))
                            .collect();
                        return LispOutput::Tokens(quote! { #name_ts ! (#(#args),*) });
                    }
                }
                "self" => {
                    // self.x.y...
                    let all: TokenStream2 = tokens.iter().cloned().collect();
                    return LispOutput::Tokens(quote! { #all });
                }
                "val" => {
                    if tokens.len() >= 2 {
                        return LispOutput::Expr(eval_lisp_arg(&tokens[1..]));
                    }
                }
                "if" => {
                    return LispOutput::Expr(eval_if(&tokens[1..]));
                }
                "cond" => {
                    return LispOutput::Expr(eval_cond(&tokens[1..]));
                }
                "match" => {
                    return LispOutput::Expr(eval_match(&tokens[1..]));
                }
                "return" => {
                    if tokens.len() == 1 {
                        return LispOutput::Expr(syn::Expr::Return(syn::ExprReturn {
                            attrs: vec![],
                            return_token: syn::token::Return::default(),
                            expr: None,
                        }));
                    }
                    let val = eval_lisp_arg(&tokens[1..]);
                    return LispOutput::Expr(syn::Expr::Return(syn::ExprReturn {
                        attrs: vec![],
                        return_token: syn::token::Return::default(),
                        expr: Some(Box::new(val)),
                    }));
                }
                "let" => {
                    return eval_let(&tokens[1..]);
                }
                "ref" => {
                    // (ref mut x) or (ref x)
                    if tokens.len() >= 3 && is_ident(&tokens[1], "mut") {
                        let e = eval_lisp_arg(&tokens[2..3]);
                        return LispOutput::Expr(syn::Expr::Reference(syn::ExprReference {
                            attrs: vec![],
                            and_token: syn::token::And::default(),
                            mutability: Some(syn::token::Mut::default()),
                            expr: Box::new(e),
                        }));
                    }
                    if tokens.len() >= 2 {
                        let e = eval_lisp_arg(&tokens[1..2]);
                        return LispOutput::Expr(syn::Expr::Reference(syn::ExprReference {
                            attrs: vec![],
                            and_token: syn::token::And::default(),
                            mutability: None,
                            expr: Box::new(e),
                        }));
                    }
                }
                "deref" => {
                    if tokens.len() >= 2 {
                        let e = eval_lisp_arg(&tokens[1..2]);
                        return LispOutput::Expr(syn::Expr::Unary(syn::ExprUnary {
                            attrs: vec![],
                            op: syn::UnOp::Deref(syn::token::Star::default()),
                            expr: Box::new(e),
                        }));
                    }
                }
                "as" => {
                    if tokens.len() >= 3 {
                        let e = eval_lisp_arg(&tokens[1..2]);
                        let typ: TokenStream2 = tokens[2..].iter().cloned().collect();
                        let ty = validate_type(typ);
                        return LispOutput::Expr(syn::Expr::Cast(syn::ExprCast {
                            attrs: vec![],
                            expr: Box::new(e),
                            as_token: syn::token::As::default(),
                            ty: Box::new(syn::parse2::<syn::Type>(ty).unwrap_or_else(|_| {
                                let fallback_ty: TokenStream2 =
                                    tokens[2..].iter().cloned().collect();
                                syn::Type::Verbatim(fallback_ty)
                            })),
                        }));
                    }
                }
                "break" => {
                    if tokens.len() == 1 {
                        return LispOutput::Expr(syn::Expr::Break(syn::ExprBreak {
                            attrs: vec![],
                            break_token: syn::token::Break::default(),
                            label: None,
                            expr: None,
                        }));
                    }
                    // Check for label: (break 'label) or (break 'label expr)
                    if tokens.len() >= 3 {
                        if let TokenTree::Punct(p) = &tokens[1] {
                            if p.as_char() == '\'' {
                                let tick = &tokens[1];
                                let label = &tokens[2];
                                if tokens.len() >= 4 {
                                    let val = eval_lisp_arg(&tokens[3..4]);
                                    return LispOutput::Tokens(quote! { break #tick #label #val });
                                }
                                return LispOutput::Tokens(quote! { break #tick #label });
                            }
                        }
                    }
                    let val = eval_lisp_arg(&tokens[1..]);
                    return LispOutput::Expr(syn::Expr::Break(syn::ExprBreak {
                        attrs: vec![],
                        break_token: syn::token::Break::default(),
                        label: None,
                        expr: Some(Box::new(val)),
                    }));
                }
                "continue" => {
                    if tokens.len() >= 3 {
                        if let TokenTree::Punct(p) = &tokens[1] {
                            if p.as_char() == '\'' {
                                let tick = &tokens[1];
                                let label = &tokens[2];
                                return LispOutput::Tokens(quote! { continue #tick #label });
                            }
                        }
                    }
                    return LispOutput::Expr(syn::Expr::Continue(syn::ExprContinue {
                        attrs: vec![],
                        continue_token: syn::token::Continue::default(),
                        label: None,
                    }));
                }
                "set" => {
                    // (set var val)
                    if tokens.len() >= 3 {
                        let var = &tokens[1];
                        let val = eval_lisp_arg(&tokens[2..3]);
                        return LispOutput::Tokens(quote! { #var = #val; });
                    }
                }
                "neg" => {
                    if tokens.len() >= 2 {
                        let e = eval_lisp_arg(&tokens[1..2]);
                        return LispOutput::Expr(syn::Expr::Unary(syn::ExprUnary {
                            attrs: vec![],
                            op: syn::UnOp::Neg(syn::token::Minus::default()),
                            expr: Box::new(e),
                        }));
                    }
                }
                "index" => {
                    if tokens.len() >= 3 {
                        let coll = eval_lisp_arg(&tokens[1..2]);
                        let key = eval_lisp_arg(&tokens[2..3]);
                        return LispOutput::Expr(syn::Expr::Index(syn::ExprIndex {
                            attrs: vec![],
                            expr: Box::new(coll),
                            bracket_token: syn::token::Bracket::default(),
                            index: Box::new(key),
                        }));
                    }
                }
                "field" => {
                    if tokens.len() >= 3 {
                        let obj = eval_lisp_arg(&tokens[1..2]);
                        let name = &tokens[2];
                        return LispOutput::Tokens(quote! { #obj . #name });
                    }
                }
                "tuple" => {
                    let elems: Vec<syn::Expr> = tokens[1..]
                        .iter()
                        .map(|t| eval_lisp_arg(std::slice::from_ref(t)))
                        .collect();
                    let mut punct_elems = syn::punctuated::Punctuated::new();
                    for e in elems {
                        punct_elems.push(e);
                    }
                    // Single-element tuples need a trailing comma:
                    // tokens.len() == 2 means ["tuple", element], i.e. one element
                    if tokens.len() == 2 {
                        punct_elems.push_punct(syn::token::Comma::default());
                    }
                    return LispOutput::Expr(syn::Expr::Tuple(syn::ExprTuple {
                        attrs: vec![],
                        paren_token: syn::token::Paren::default(),
                        elems: punct_elems,
                    }));
                }
                "array" => {
                    let elems: Vec<syn::Expr> = tokens[1..]
                        .iter()
                        .map(|t| eval_lisp_arg(std::slice::from_ref(t)))
                        .collect();
                    let mut punct_elems = syn::punctuated::Punctuated::new();
                    for e in elems {
                        punct_elems.push(e);
                    }
                    return LispOutput::Expr(syn::Expr::Array(syn::ExprArray {
                        attrs: vec![],
                        bracket_token: syn::token::Bracket::default(),
                        elems: punct_elems,
                    }));
                }
                "vec" => {
                    let args: Vec<syn::Expr> = tokens[1..]
                        .iter()
                        .map(|t| eval_lisp_arg(std::slice::from_ref(t)))
                        .collect();
                    return LispOutput::Tokens(quote! { vec![#(#args),*] });
                }
                "len" => {
                    if tokens.len() >= 2 {
                        let e = eval_lisp_arg(&tokens[1..2]);
                        return LispOutput::Tokens(quote! { #e.len() });
                    }
                }
                "await" => {
                    if tokens.len() >= 2 {
                        let e = eval_lisp_arg(&tokens[1..2]);
                        return LispOutput::Expr(syn::Expr::Await(syn::ExprAwait {
                            attrs: vec![],
                            base: Box::new(e),
                            dot_token: syn::token::Dot::default(),
                            await_token: syn::token::Await::default(),
                        }));
                    }
                }
                "false" => {
                    return LispOutput::Expr(syn::Expr::Lit(syn::ExprLit {
                        attrs: vec![],
                        lit: syn::Lit::Bool(syn::LitBool {
                            value: false,
                            span: id.span(),
                        }),
                    }));
                }
                "true" => {
                    return LispOutput::Expr(syn::Expr::Lit(syn::ExprLit {
                        attrs: vec![],
                        lit: syn::Lit::Bool(syn::LitBool {
                            value: true,
                            span: id.span(),
                        }),
                    }));
                }
                "fn" => {
                    return LispOutput::Expr(eval_closure(&tokens[1..]));
                }
                "panic" => {
                    let args: Vec<syn::Expr> = tokens[1..]
                        .iter()
                        .map(|t| eval_lisp_arg(std::slice::from_ref(t)))
                        .collect();
                    return LispOutput::Tokens(quote! { panic!(#(#args),*) });
                }
                "unsafe" => {
                    let mut items = Vec::new();
                    for tt in &tokens[1..] {
                        if let TokenTree::Group(g) = tt {
                            if g.delimiter() == Delimiter::Parenthesis {
                                let inner: Vec<TokenTree> = g.stream().into_iter().collect();
                                items.push(eval_lisp_expr(&inner));
                            }
                        }
                    }
                    return LispOutput::Expr(syn::Expr::Unsafe(syn::ExprUnsafe {
                        attrs: vec![],
                        unsafe_token: syn::token::Unsafe::default(),
                        block: syn::Block {
                            brace_token: syn::token::Brace::default(),
                            stmts: build_block_stmts(items),
                        },
                    }));
                }
                "block" => {
                    let mut start = 1;
                    let mut label = None;
                    // Check for label: (block 'label body...)
                    if tokens.len() >= 3 {
                        if let TokenTree::Punct(p) = &tokens[1] {
                            if p.as_char() == '\'' {
                                if let TokenTree::Ident(lbl) = &tokens[2] {
                                    label = Some(syn::Label {
                                        name: syn::Lifetime::new(&format!("'{}", lbl), lbl.span()),
                                        colon_token: syn::token::Colon::default(),
                                    });
                                    start = 3;
                                }
                            }
                        }
                    }
                    let mut items = Vec::new();
                    for tt in &tokens[start..] {
                        if let TokenTree::Group(g) = tt {
                            if g.delimiter() == Delimiter::Parenthesis {
                                let inner: Vec<TokenTree> = g.stream().into_iter().collect();
                                items.push(eval_lisp_expr(&inner));
                            }
                        }
                    }
                    return LispOutput::Expr(syn::Expr::Block(syn::ExprBlock {
                        attrs: vec![],
                        label,
                        block: syn::Block {
                            brace_token: syn::token::Brace::default(),
                            stmts: build_block_stmts(items),
                        },
                    }));
                }
                "for" => {
                    return LispOutput::Expr(eval_for(&tokens[1..]));
                }
                "while" => {
                    return LispOutput::Expr(eval_while(&tokens[1..]));
                }
                "loop" => {
                    let mut items = Vec::new();
                    for tt in &tokens[1..] {
                        if let TokenTree::Group(g) = tt {
                            if g.delimiter() == Delimiter::Parenthesis {
                                let inner: Vec<TokenTree> = g.stream().into_iter().collect();
                                items.push(eval_lisp_expr(&inner));
                            }
                        }
                    }
                    return LispOutput::Expr(syn::Expr::Loop(syn::ExprLoop {
                        attrs: vec![],
                        label: None,
                        loop_token: syn::token::Loop::default(),
                        body: syn::Block {
                            brace_token: syn::token::Brace::default(),
                            stmts: build_block_stmts(items),
                        },
                    }));
                }
                _ => {}
            }
        }
        TokenTree::Punct(_) => {
            if let Some(result) = eval_punct_expr(tokens) {
                return result;
            }
        }
        _ => {}
    }

    // Check for ident! pattern (macro invocation shorthand: (name! args...) → name!(args...))
    if tokens.len() >= 2 {
        if let TokenTree::Ident(macro_id) = &tokens[0] {
            if is_punct(&tokens[1], '!') {
                let path = syn::Path {
                    leading_colon: None,
                    segments: {
                        let mut s = syn::punctuated::Punctuated::new();
                        s.push(syn::PathSegment {
                            ident: macro_id.clone(),
                            arguments: syn::PathArguments::None,
                        });
                        s
                    },
                };
                let args: Vec<syn::Expr> = tokens[2..]
                    .iter()
                    .map(|t| eval_lisp_arg(std::slice::from_ref(t)))
                    .collect();
                return LispOutput::Expr(make_macro_call(path, args));
            }
        }
    }

    // Check for ident.ident pattern (method call or field access)
    if tokens.len() >= 3 {
        if let TokenTree::Ident(first) = &tokens[0] {
            if is_punct(&tokens[1], '.') {
                // Could be field access chain or method call
                // Build chain of ident.ident.ident... as field accesses, then check for args
                let first_expr = eval_lisp_arg(std::slice::from_ref(&tokens[0]));
                let mut chain_expr = first_expr;
                let mut path_end = 2;
                // Build field/method chain
                while path_end < tokens.len() {
                    if let TokenTree::Ident(field_id) = &tokens[path_end] {
                        chain_expr = make_field_access(chain_expr, field_id);
                        path_end += 1;
                        // Check for next dot
                        if path_end < tokens.len() && is_punct(&tokens[path_end], '.') {
                            path_end += 1;
                            continue;
                        }
                    }
                    break;
                }
                if path_end < tokens.len() {
                    // Has remaining args → method call on the last field
                    // We need to "undo" the last field access and turn it into a method call
                    if let syn::Expr::Field(field_expr) = chain_expr {
                        if let syn::Member::Named(method_ident) = field_expr.member {
                            let args: Vec<syn::Expr> = tokens[path_end..]
                                .iter()
                                .map(|t| eval_lisp_arg(std::slice::from_ref(t)))
                                .collect();
                            return LispOutput::Expr(make_method_call(
                                *field_expr.base,
                                method_ident,
                                args,
                            ));
                        }
                    }
                    // Fallback for complex cases
                    let path_ts: TokenStream2 = tokens[..path_end].iter().cloned().collect();
                    let args: Vec<syn::Expr> = tokens[path_end..]
                        .iter()
                        .map(|t| eval_lisp_arg(std::slice::from_ref(t)))
                        .collect();
                    return LispOutput::Tokens(quote! { #path_ts(#(#args),*) });
                } else {
                    // No args
                    // `self.x.y` is field access
                    if *first == "self" {
                        return LispOutput::Expr(chain_expr);
                    }
                    // Non-self: each .ident is a zero-arg method call
                    // Rebuild as method call chain
                    let first_expr2 = eval_lisp_arg(std::slice::from_ref(&tokens[0]));
                    let mut result = first_expr2;
                    let mut j = 2; // skip first ident and first dot
                    while j < tokens.len() {
                        if let TokenTree::Ident(method_id) = &tokens[j] {
                            result = make_method_call(result, method_id.clone(), vec![]);
                        }
                        j += 2; // skip ident and next dot
                    }
                    return LispOutput::Expr(result);
                }
            }
        }
    }

    // Check for path::call pattern
    if tokens.len() >= 3 {
        if let TokenTree::Ident(_) = &tokens[0] {
            if is_punct(&tokens[1], ':') && tokens.len() > 2 && is_punct(&tokens[2], ':') {
                // Path call or path::macro!
                let (path, rest) = consume_type_path(tokens);
                let path_ts: TokenStream2 = path.into_iter().collect();
                if rest.is_empty() {
                    // In the S-expression DSL, (path::func) is a zero-arg function call
                    let func = syn::parse2::<syn::Expr>(path_ts.clone())
                        .unwrap_or(syn::Expr::Verbatim(path_ts));
                    return LispOutput::Expr(make_call(func, vec![]));
                }
                // Check for path::name! pattern (macro invocation)
                if !rest.is_empty() && is_punct(&rest[0], '!') {
                    if let Ok(path) = syn::parse2::<syn::Path>(path_ts.clone()) {
                        let args: Vec<syn::Expr> = rest[1..]
                            .iter()
                            .map(|t| eval_lisp_arg(std::slice::from_ref(t)))
                            .collect();
                        return LispOutput::Expr(make_macro_call(path, args));
                    }
                    // Fallback
                    let args: Vec<syn::Expr> = rest[1..]
                        .iter()
                        .map(|t| eval_lisp_arg(std::slice::from_ref(t)))
                        .collect();
                    return LispOutput::Tokens(quote! { #path_ts ! (#(#args),*) });
                }
                let func = syn::parse2::<syn::Expr>(path_ts.clone())
                    .unwrap_or(syn::Expr::Verbatim(path_ts));
                let args: Vec<syn::Expr> = rest
                    .iter()
                    .map(|t| eval_lisp_arg(std::slice::from_ref(t)))
                    .collect();
                return LispOutput::Expr(make_call(func, args));
            }
        }
    }

    // Default: treat first ident as function call
    if let TokenTree::Ident(id) = &tokens[0] {
        let func = syn::Expr::Path(syn::ExprPath {
            attrs: vec![],
            qself: None,
            path: syn::Path {
                leading_colon: None,
                segments: {
                    let mut s = syn::punctuated::Punctuated::new();
                    s.push(syn::PathSegment {
                        ident: id.clone(),
                        arguments: syn::PathArguments::None,
                    });
                    s
                },
            },
        });
        let args: Vec<syn::Expr> = tokens[1..]
            .iter()
            .map(|t| eval_lisp_arg(std::slice::from_ref(t)))
            .collect();
        return LispOutput::Expr(make_call(func, args));
    }

    // Fallback: emit tokens as-is
    let ts: TokenStream2 = tokens.iter().cloned().collect();
    LispOutput::Tokens(quote! { #ts })
}

pub(crate) fn eval_lisp_arg(tokens: &[TokenTree]) -> syn::Expr {
    if tokens.len() == 1 {
        if let TokenTree::Group(g) = &tokens[0] {
            if g.delimiter() == Delimiter::Parenthesis {
                return eval_lisp_expr(&g.stream().into_iter().collect::<Vec<_>>()).into_expr();
            }
        }
        let ts: TokenStream2 = std::iter::once(tokens[0].clone()).collect();
        return syn::parse2::<syn::Expr>(ts.clone()).unwrap_or(syn::Expr::Verbatim(ts));
    }
    // Multi-token: try syn::parse2 before falling back to Verbatim
    let ts: TokenStream2 = tokens.iter().cloned().collect();
    syn::parse2::<syn::Expr>(ts.clone()).unwrap_or(syn::Expr::Verbatim(ts))
}
