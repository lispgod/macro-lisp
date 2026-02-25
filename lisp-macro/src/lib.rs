use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use proc_macro2::{Delimiter, TokenTree, Spacing, Punct, Span};
use quote::quote;

// ─── Helper: check if a token tree is a specific punctuation character ───
fn is_punct(tt: &TokenTree, ch: char) -> bool {
    matches!(tt, TokenTree::Punct(p) if p.as_char() == ch)
}

// ─── Helper: check if a token tree is a specific ident ───
fn is_ident(tt: &TokenTree, name: &str) -> bool {
    matches!(tt, TokenTree::Ident(i) if *i == name)
}

// ─── Helper: consume balanced angle brackets from tokens, returning (angle_contents, rest) ───
// Expects tokens[0] to be '<'
fn consume_angle_brackets(tokens: &[TokenTree]) -> (Vec<TokenTree>, &[TokenTree]) {
    let mut depth = 0i32;
    let mut i = 0;
    let mut contents = Vec::new();

    while i < tokens.len() {
        let tt = &tokens[i];
        match tt {
            TokenTree::Punct(p) if p.as_char() == '<' => {
                if depth == 0 {
                    depth = 1;
                    i += 1;
                    continue;
                }
                depth += 1;
                contents.push(tt.clone());
            }
            TokenTree::Punct(p) if p.as_char() == '>' => {
                depth -= 1;
                if depth == 0 {
                    return (contents, &tokens[i + 1..]);
                }
                contents.push(tt.clone());
            }
            _ => {
                if depth > 0 {
                    contents.push(tt.clone());
                }
            }
        }
        i += 1;
    }
    (contents, &tokens[tokens.len()..])
}

// ─── Helper: consume a type path (like `core::fmt::Display` or `Name<T>`) ───
// Returns (path_tokens, rest)
fn consume_type_path(tokens: &[TokenTree]) -> (Vec<TokenTree>, &[TokenTree]) {
    let mut result = Vec::new();
    let mut i = 0;

    while i < tokens.len() {
        let tt = &tokens[i];
        match tt {
            TokenTree::Ident(_) => {
                result.push(tt.clone());
                i += 1;
                // Check for :: after ident
                if i + 1 < tokens.len() {
                    if is_punct(&tokens[i], ':') && i + 1 < tokens.len() && is_punct(&tokens[i + 1], ':') {
                        result.push(tokens[i].clone());
                        result.push(tokens[i + 1].clone());
                        i += 2;
                        continue;
                    }
                }
                // Check for <...> after ident
                if i < tokens.len() && is_punct(&tokens[i], '<') {
                    result.push(tokens[i].clone()); // <
                    let (angle_contents, rest) = consume_angle_brackets(&tokens[i..]);
                    for t in &angle_contents {
                        result.push(t.clone());
                    }
                    // Add the closing >
                    result.push(TokenTree::Punct(Punct::new('>', Spacing::Alone)));
                    let consumed = tokens.len() - rest.len();
                    i = consumed;
                }
                break;
            }
            _ => break,
        }
    }
    (result, &tokens[i..])
}

/// Evaluate a lisp expression (the contents of a parenthesized group) into Rust code.
/// This is a mini-evaluator that handles the most common patterns directly,
/// avoiding the need to delegate to `::lisp::lisp!` (which has hygiene issues with `self`).
fn eval_lisp_expr(tokens: &[TokenTree]) -> TokenStream2 {
    if tokens.is_empty() {
        return quote! {};
    }

    // Single token — return as-is
    if tokens.len() == 1 {
        let t = &tokens[0];
        // If it's a parenthesized group, recurse
        if let TokenTree::Group(g) = t {
            if g.delimiter() == Delimiter::Parenthesis {
                return eval_lisp_expr(&g.stream().into_iter().collect::<Vec<_>>());
            }
        }
        return quote! { #t };
    }

    // Check first token for special forms
    match &tokens[0] {
        TokenTree::Ident(id) => {
            let name = id.to_string();
            match name.as_str() {
                "new" => {
                    // (new Name (field val) ...)
                    if tokens.len() >= 2 {
                        let struct_name = &tokens[1];
                        let mut fields = Vec::new();
                        for tt in &tokens[2..] {
                            if let TokenTree::Group(g) = tt {
                                if g.delimiter() == Delimiter::Parenthesis {
                                    let inner: Vec<TokenTree> = g.stream().into_iter().collect();
                                    if inner.len() >= 2 {
                                        let fname = &inner[0];
                                        let fval = eval_lisp_arg(&inner[1..]);
                                        fields.push(quote! { #fname: #fval });
                                    }
                                }
                            }
                        }
                        return quote! { #struct_name { #(#fields),* } };
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
                        let args: Vec<TokenStream2> = macro_name_tokens[mi..]
                            .iter()
                            .map(|t| eval_lisp_arg(std::slice::from_ref(t)))
                            .collect();
                        return quote! { #name_ts ! (#(#args),*) };
                    }
                }
                "self" => {
                    // self.x.y...
                    let all: TokenStream2 = tokens.iter().cloned().collect();
                    return quote! { #all };
                }
                "val" => {
                    if tokens.len() == 2 {
                        return eval_lisp_arg(&tokens[1..]);
                    }
                }
                "if" => {
                    return eval_if(&tokens[1..]);
                }
                "match" => {
                    // Pass through to lisp! for complex matching
                    let rest: TokenStream2 = tokens.iter().cloned().collect();
                    return quote! { ::lisp::lisp!(#rest) };
                }
                "return" => {
                    if tokens.len() == 1 {
                        return quote! { return };
                    }
                    let val = eval_lisp_arg(&tokens[1..]);
                    return quote! { return #val };
                }
                "let" => {
                    return eval_let(&tokens[1..]);
                }
                "rust" => {
                    // (rust { code }) or (rust stmt...)
                    if tokens.len() >= 2 {
                        if let TokenTree::Group(g) = &tokens[1] {
                            if g.delimiter() == Delimiter::Brace {
                                let inner = g.stream();
                                return quote! { { #inner } };
                            }
                        }
                    }
                    // (rust stmt...) — pass through as raw Rust
                    let rest: TokenStream2 = tokens[1..].iter().cloned().collect();
                    return quote! { #rest };
                }
                "ref" => {
                    // (ref mut x) or (ref x)
                    if tokens.len() >= 3 && is_ident(&tokens[1], "mut") {
                        let e = eval_lisp_arg(&tokens[2..3]);
                        return quote! { &mut #e };
                    }
                    if tokens.len() >= 2 {
                        let e = eval_lisp_arg(&tokens[1..2]);
                        return quote! { & #e };
                    }
                }
                "deref" => {
                    if tokens.len() >= 2 {
                        let e = eval_lisp_arg(&tokens[1..2]);
                        return quote! { * #e };
                    }
                }
                "as" => {
                    if tokens.len() >= 3 {
                        let e = eval_lisp_arg(&tokens[1..2]);
                        let typ: TokenStream2 = tokens[2..].iter().cloned().collect();
                        return quote! { #e as #typ };
                    }
                }
                "break" => {
                    if tokens.len() == 1 {
                        return quote! { break; };
                    }
                    let val = eval_lisp_arg(&tokens[1..]);
                    return quote! { break #val; };
                }
                "continue" => {
                    return quote! { continue; };
                }
                "set" => {
                    // (set var val)
                    if tokens.len() >= 3 {
                        let var = &tokens[1];
                        let val = eval_lisp_arg(&tokens[2..3]);
                        return quote! { #var = #val; };
                    }
                }
                "neg" => {
                    if tokens.len() >= 2 {
                        let e = eval_lisp_arg(&tokens[1..2]);
                        return quote! { - #e };
                    }
                }
                "index" => {
                    if tokens.len() >= 3 {
                        let coll = eval_lisp_arg(&tokens[1..2]);
                        let key = eval_lisp_arg(&tokens[2..3]);
                        return quote! { #coll[#key] };
                    }
                }
                "field" => {
                    if tokens.len() >= 3 {
                        let obj = eval_lisp_arg(&tokens[1..2]);
                        let name = &tokens[2];
                        return quote! { #obj . #name };
                    }
                }
                "tuple" => {
                    if tokens.len() == 2 {
                        let e = eval_lisp_arg(&tokens[1..2]);
                        return quote! { (#e,) };
                    }
                    let args: Vec<TokenStream2> = tokens[1..]
                        .iter()
                        .map(|t| eval_lisp_arg(std::slice::from_ref(t)))
                        .collect();
                    return quote! { (#(#args),*) };
                }
                "array" => {
                    let args: Vec<TokenStream2> = tokens[1..]
                        .iter()
                        .map(|t| eval_lisp_arg(std::slice::from_ref(t)))
                        .collect();
                    return quote! { [#(#args),*] };
                }
                "vec" => {
                    let args: Vec<TokenStream2> = tokens[1..]
                        .iter()
                        .map(|t| eval_lisp_arg(std::slice::from_ref(t)))
                        .collect();
                    return quote! { vec![#(#args),*] };
                }
                "len" => {
                    if tokens.len() >= 2 {
                        let e = eval_lisp_arg(&tokens[1..2]);
                        return quote! { #e.len() };
                    }
                }
                "await" => {
                    if tokens.len() >= 2 {
                        let e = eval_lisp_arg(&tokens[1..2]);
                        return quote! { #e.await };
                    }
                }
                "false" => { return quote! { false }; }
                "true" => { return quote! { true }; }
                "block" => {
                    let mut stmts = Vec::new();
                    for tt in &tokens[1..] {
                        if let TokenTree::Group(g) = tt {
                            if g.delimiter() == Delimiter::Parenthesis {
                                let inner: Vec<TokenTree> = g.stream().into_iter().collect();
                                stmts.push(eval_lisp_expr(&inner));
                            }
                        }
                    }
                    return quote! { { #(#stmts);* } };
                }
                "for" => {
                    return eval_for(&tokens[1..]);
                }
                "while" => {
                    return eval_while(&tokens[1..]);
                }
                "loop" => {
                    let mut stmts = Vec::new();
                    for tt in &tokens[1..] {
                        if let TokenTree::Group(g) = tt {
                            if g.delimiter() == Delimiter::Parenthesis {
                                let inner: Vec<TokenTree> = g.stream().into_iter().collect();
                                stmts.push(eval_lisp_expr(&inner));
                            }
                        }
                    }
                    return quote! { loop { #(#stmts);* } };
                }
                _ => {}
            }
        }
        TokenTree::Punct(p) => {
            let ch = p.as_char();
            match ch {
                '+' => {
                    // Check for +=
                    if tokens.len() >= 3 && is_punct(&tokens[1], '=') {
                        let lhs: TokenStream2 = tokens[2..tokens.len()-1].iter().cloned().collect();
                        let rhs = eval_lisp_arg(&tokens[tokens.len()-1..]);
                        return quote! { #lhs += #rhs; };
                    }
                    if tokens.len() == 3 {
                        let a = eval_lisp_arg(&tokens[1..2]);
                        let b = eval_lisp_arg(&tokens[2..3]);
                        return quote! { #a + #b };
                    } else if tokens.len() > 3 {
                        let a = eval_lisp_arg(&tokens[1..2]);
                        let b = eval_lisp_arg(&tokens[2..3]);
                        let mut result = quote! { #a + #b };
                        for t in &tokens[3..] {
                            let c = eval_lisp_arg(std::slice::from_ref(t));
                            result = quote! { #result + #c };
                        }
                        return result;
                    }
                }
                '-' => {
                    // Check for -=
                    if tokens.len() >= 3 && is_punct(&tokens[1], '=') {
                        let lhs: TokenStream2 = tokens[2..tokens.len()-1].iter().cloned().collect();
                        let rhs = eval_lisp_arg(&tokens[tokens.len()-1..]);
                        return quote! { #lhs -= #rhs; };
                    }
                    if tokens.len() == 3 {
                        let a = eval_lisp_arg(&tokens[1..2]);
                        let b = eval_lisp_arg(&tokens[2..3]);
                        return quote! { #a - #b };
                    } else if tokens.len() > 3 {
                        let a = eval_lisp_arg(&tokens[1..2]);
                        let b = eval_lisp_arg(&tokens[2..3]);
                        let mut result = quote! { #a - #b };
                        for t in &tokens[3..] {
                            let c = eval_lisp_arg(std::slice::from_ref(t));
                            result = quote! { #result - #c };
                        }
                        return result;
                    }
                }
                '*' => {
                    // Check for *=
                    if tokens.len() >= 3 && is_punct(&tokens[1], '=') {
                        let lhs: TokenStream2 = tokens[2..tokens.len()-1].iter().cloned().collect();
                        let rhs = eval_lisp_arg(&tokens[tokens.len()-1..]);
                        return quote! { #lhs *= #rhs; };
                    }
                    if tokens.len() == 3 {
                        let a = eval_lisp_arg(&tokens[1..2]);
                        let b = eval_lisp_arg(&tokens[2..3]);
                        return quote! { #a * #b };
                    }
                }
                '/' => {
                    // Check for /=
                    if tokens.len() >= 3 && is_punct(&tokens[1], '=') {
                        let lhs: TokenStream2 = tokens[2..tokens.len()-1].iter().cloned().collect();
                        let rhs = eval_lisp_arg(&tokens[tokens.len()-1..]);
                        return quote! { #lhs /= #rhs; };
                    }
                    if tokens.len() == 3 {
                        let a = eval_lisp_arg(&tokens[1..2]);
                        let b = eval_lisp_arg(&tokens[2..3]);
                        return quote! { #a / #b };
                    }
                }
                '%' => {
                    // Check for %=
                    if tokens.len() >= 3 && is_punct(&tokens[1], '=') {
                        let lhs: TokenStream2 = tokens[2..tokens.len()-1].iter().cloned().collect();
                        let rhs = eval_lisp_arg(&tokens[tokens.len()-1..]);
                        return quote! { #lhs %= #rhs; };
                    }
                    if tokens.len() == 3 {
                        let a = eval_lisp_arg(&tokens[1..2]);
                        let b = eval_lisp_arg(&tokens[2..3]);
                        return quote! { #a % #b };
                    }
                }
                '>' => {
                    // Check for >= or >>
                    if tokens.len() >= 3 && is_punct(&tokens[1], '=') {
                        let a = eval_lisp_arg(&tokens[2..3]);
                        let b = eval_lisp_arg(&tokens[3..4]);
                        return quote! { #a >= #b };
                    }
                    if tokens.len() == 3 {
                        let a = eval_lisp_arg(&tokens[1..2]);
                        let b = eval_lisp_arg(&tokens[2..3]);
                        return quote! { #a > #b };
                    }
                }
                '<' => {
                    if tokens.len() >= 3 && is_punct(&tokens[1], '=') {
                        let a = eval_lisp_arg(&tokens[2..3]);
                        let b = eval_lisp_arg(&tokens[3..4]);
                        return quote! { #a <= #b };
                    }
                    if tokens.len() == 3 {
                        let a = eval_lisp_arg(&tokens[1..2]);
                        let b = eval_lisp_arg(&tokens[2..3]);
                        return quote! { #a < #b };
                    }
                }
                '=' => {
                    if tokens.len() >= 3 {
                        if is_punct(&tokens[1], '=') {
                            // ==
                            let a = eval_lisp_arg(&tokens[2..3]);
                            let b = eval_lisp_arg(&tokens[3..4]);
                            return quote! { #a == #b };
                        }
                        // assignment
                        let lhs: TokenStream2 = tokens[1..tokens.len()-1].iter().cloned().collect();
                        let rhs = eval_lisp_arg(&tokens[tokens.len()-1..]);
                        return quote! { #lhs = #rhs; };
                    }
                }
                '!' => {
                    if tokens.len() >= 2 && is_punct(&tokens[1], '=') {
                        // !=
                        let a = eval_lisp_arg(&tokens[2..3]);
                        let b = eval_lisp_arg(&tokens[3..4]);
                        return quote! { #a != #b };
                    }
                    if tokens.len() == 2 {
                        let e = eval_lisp_arg(&tokens[1..2]);
                        return quote! { ! #e };
                    }
                }
                '.' => {
                    // (. obj field1 field2 ...)
                    if tokens.len() >= 3 {
                        let obj = eval_lisp_arg(&tokens[1..2]);
                        let fields: Vec<&TokenTree> = tokens[2..].iter().collect();
                        let fields_ts: TokenStream2 = fields.iter().map(|f| {
                            let f = (*f).clone();
                            quote! { .#f }
                        }).collect();
                        return quote! { #obj #fields_ts };
                    }
                }
                '&' => {
                    if tokens.len() >= 2 && is_punct(&tokens[1], '&') {
                        // &&
                        let a = eval_lisp_arg(&tokens[2..3]);
                        let b = eval_lisp_arg(&tokens[3..4]);
                        return quote! { #a && #b };
                    }
                }
                '|' => {
                    if tokens.len() >= 2 && is_punct(&tokens[1], '|') {
                        // ||
                        let a = eval_lisp_arg(&tokens[2..3]);
                        let b = eval_lisp_arg(&tokens[3..4]);
                        return quote! { #a || #b };
                    }
                }
                '?' => {
                    if tokens.len() == 2 {
                        let e = eval_lisp_arg(&tokens[1..2]);
                        return quote! { #e? };
                    }
                }
                _ => {}
            }
        }
        _ => {}
    }

    // Check for ident.ident pattern (method call or field access)
    if tokens.len() >= 3 {
        if let TokenTree::Ident(first) = &tokens[0] {
            if is_punct(&tokens[1], '.') {
                // Could be field access chain or method call
                // Find extent of the path: ident.ident.ident...
                let mut path_end = 1;
                while path_end + 1 < tokens.len() {
                    if is_punct(&tokens[path_end], '.') {
                        if let TokenTree::Ident(_) = &tokens[path_end + 1] {
                            path_end += 2;
                            continue;
                        }
                    }
                    break;
                }
                let path_ts: TokenStream2 = tokens[..path_end].iter().cloned().collect();
                if path_end < tokens.len() {
                    // Has args - method call
                    let args: Vec<TokenStream2> = tokens[path_end..]
                        .iter()
                        .map(|t| eval_lisp_arg(std::slice::from_ref(t)))
                        .collect();
                    return quote! { #path_ts(#(#args),*) };
                } else {
                    // No args — method call with zero args (like the original catch-all)
                    // EXCEPT for `self.x.y` which is field access
                    if first.to_string() == "self" {
                        return quote! { #path_ts };
                    }
                    return quote! { #path_ts() };
                }
            }
        }
    }

    // Check for path::call pattern
    if tokens.len() >= 3 {
        if let TokenTree::Ident(_) = &tokens[0] {
            if is_punct(&tokens[1], ':') && tokens.len() > 2 && is_punct(&tokens[2], ':') {
                // Path call
                let (path, rest) = consume_type_path(tokens);
                let path_ts: TokenStream2 = path.into_iter().collect();
                if rest.is_empty() {
                    return quote! { #path_ts };
                }
                let args: Vec<TokenStream2> = rest
                    .iter()
                    .map(|t| eval_lisp_arg(std::slice::from_ref(t)))
                    .collect();
                return quote! { #path_ts(#(#args),*) };
            }
        }
    }

    // Default: treat first ident as function call
    if let TokenTree::Ident(id) = &tokens[0] {
        let args: Vec<TokenStream2> = tokens[1..]
            .iter()
            .map(|t| eval_lisp_arg(std::slice::from_ref(t)))
            .collect();
        return quote! { #id(#(#args),*) };
    }

    // Fallback: emit tokens as-is
    let ts: TokenStream2 = tokens.iter().cloned().collect();
    quote! { #ts }
}

fn eval_lisp_arg(tokens: &[TokenTree]) -> TokenStream2 {
    if tokens.len() == 1 {
        if let TokenTree::Group(g) = &tokens[0] {
            if g.delimiter() == Delimiter::Parenthesis {
                return eval_lisp_expr(&g.stream().into_iter().collect::<Vec<_>>());
            }
        }
        let t = &tokens[0];
        return quote! { #t };
    }
    // Multi-token: treat as expression
    let ts: TokenStream2 = tokens.iter().cloned().collect();
    quote! { #ts }
}

fn eval_if(tokens: &[TokenTree]) -> TokenStream2 {
    if tokens.is_empty() { return quote! {}; }

    let cond = eval_lisp_arg(&tokens[0..1]);
    if tokens.len() >= 3 {
        let then_branch = eval_lisp_arg(&tokens[1..2]);
        let else_branch = eval_lisp_arg(&tokens[2..3]);
        quote! { if #cond { #then_branch } else { #else_branch } }
    } else if tokens.len() == 2 {
        let then_branch = eval_lisp_arg(&tokens[1..2]);
        quote! { if #cond { #then_branch } }
    } else {
        quote! { if #cond {} }
    }
}

fn eval_let(tokens: &[TokenTree]) -> TokenStream2 {
    if tokens.is_empty() { return quote! {}; }

    // (let mut var val) or (let var val)
    let mut i = 0;
    let is_mut = is_ident(&tokens[i], "mut");
    if is_mut { i += 1; }

    if i >= tokens.len() { return quote! {}; }

    // Check for typed let: (let (var Type) val)
    if let TokenTree::Group(g) = &tokens[i] {
        if g.delimiter() == Delimiter::Parenthesis {
            let inner: Vec<TokenTree> = g.stream().into_iter().collect();
            if inner.len() >= 2 {
                let var_name = &inner[0];
                let var_type: TokenStream2 = inner[1..].iter().cloned().collect();
                i += 1;
                if i < tokens.len() {
                    let val = eval_lisp_arg(&tokens[i..i+1]);
                    if is_mut {
                        return quote! { let mut #var_name: #var_type = #val; };
                    } else {
                        return quote! { let #var_name: #var_type = #val; };
                    }
                }
            }
        }
    }

    let var_name = &tokens[i];
    i += 1;
    if i < tokens.len() {
        let val = eval_lisp_arg(&tokens[i..i+1]);
        if is_mut {
            quote! { let mut #var_name = #val; }
        } else {
            quote! { let #var_name = #val; }
        }
    } else {
        if is_mut {
            quote! { let mut #var_name; }
        } else {
            quote! { let #var_name; }
        }
    }
}

fn eval_for(tokens: &[TokenTree]) -> TokenStream2 {
    // (for var in iter (body)...)
    if tokens.len() < 3 { return quote! {}; }
    let var = &tokens[0];
    // tokens[1] should be `in`
    let iter_expr = eval_lisp_arg(&tokens[2..3]);
    let mut body = Vec::new();
    for tt in &tokens[3..] {
        if let TokenTree::Group(g) = tt {
            if g.delimiter() == Delimiter::Parenthesis {
                let inner: Vec<TokenTree> = g.stream().into_iter().collect();
                body.push(eval_lisp_expr(&inner));
            }
        }
    }
    quote! { for #var in #iter_expr { #(#body);* } }
}

fn eval_while(tokens: &[TokenTree]) -> TokenStream2 {
    if tokens.is_empty() { return quote! {}; }
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
    quote! { while #cond { #(#body);* } }
}

/// Parse a lisp-style method/fn definition from tokens inside an impl/trait body.
/// Input: the contents of a (...) group, e.g. `fn foo ((self &Self) (x i32)) RetType (body)`
/// Also handles: `type Name = Type`
/// Returns the Rust code as TokenStream2.
fn parse_impl_body_item(tokens: &[TokenTree]) -> syn::Result<TokenStream2> {
    if tokens.is_empty() {
        return Ok(quote! {});
    }

    // Check for `type Name = Type`
    if is_ident(&tokens[0], "type") {
        let rest: TokenStream2 = tokens[1..].iter().cloned().collect();
        return Ok(quote! { type #rest; });
    }

    // Parse fn definition
    let mut i = 0;

    // Collect attributes
    let mut attrs = Vec::new();
    while i < tokens.len() {
        if let TokenTree::Punct(p) = &tokens[i] {
            if p.as_char() == '#' {
                attrs.push(tokens[i].clone());
                i += 1;
                if i < tokens.len() {
                    attrs.push(tokens[i].clone());
                    i += 1;
                }
                continue;
            }
        }
        break;
    }
    let attrs_ts: TokenStream2 = attrs.into_iter().collect();

    // Collect qualifiers: pub, const, unsafe, async
    let mut qualifiers = Vec::new();
    loop {
        if i >= tokens.len() { break; }
        match &tokens[i] {
            TokenTree::Ident(id) if *id == "pub" || *id == "const" || *id == "unsafe" || *id == "async" => {
                qualifiers.push(tokens[i].clone());
                i += 1;
            }
            TokenTree::Ident(id) if *id == "extern" => {
                qualifiers.push(tokens[i].clone());
                i += 1;
                if i < tokens.len() {
                    if let TokenTree::Literal(_) = &tokens[i] {
                        qualifiers.push(tokens[i].clone());
                        i += 1;
                    }
                }
            }
            _ => break,
        }
    }
    let quals_ts: TokenStream2 = qualifiers.into_iter().collect();

    // Skip `fn` keyword
    if i < tokens.len() && is_ident(&tokens[i], "fn") {
        i += 1;
    } else {
        // Not a fn definition — pass through to lisp!
        let all: TokenStream2 = tokens.iter().cloned().collect();
        return Ok(quote! { ::lisp::lisp! { #all } });
    }

    // Function name
    let fn_name = if i < tokens.len() {
        if let TokenTree::Ident(id) = &tokens[i] {
            let id = id.clone();
            i += 1;
            id
        } else {
            return Err(syn::Error::new(Span::call_site(), "expected function name"));
        }
    } else {
        return Err(syn::Error::new(Span::call_site(), "expected function name"));
    };

    // Check for <...> generic params
    let mut generics = None;
    if i < tokens.len() && is_punct(&tokens[i], '<') {
        let (angle_contents, rest) = consume_angle_brackets(&tokens[i..]);
        let angle_ts: TokenStream2 = angle_contents.into_iter().collect();
        generics = Some(angle_ts);
        i = tokens.len() - rest.len();
    }

    // Parse parameter list: a parenthesized group containing (name Type) pairs
    let mut params = Vec::new();
    if i < tokens.len() {
        if let TokenTree::Group(g) = &tokens[i] {
            if g.delimiter() == Delimiter::Parenthesis {
                let param_tokens: Vec<TokenTree> = g.stream().into_iter().collect();
                for tt in &param_tokens {
                    if let TokenTree::Group(pg) = tt {
                        if pg.delimiter() == Delimiter::Parenthesis {
                            let inner: Vec<TokenTree> = pg.stream().into_iter().collect();
                            if inner.len() >= 2 {
                                let param_name = &inner[0];
                                let param_type: TokenStream2 = inner[1..].iter().cloned().collect();
                                params.push(quote! { #param_name: #param_type });
                            }
                        }
                    }
                }
                i += 1;
            }
        }
    }

    // Parse return type: everything until `where`, a parenthesized group (body start), or a literal
    let mut return_type_tokens = Vec::new();
    while i < tokens.len() {
        if is_ident(&tokens[i], "where") { break; }
        if let TokenTree::Group(g) = &tokens[i] {
            if g.delimiter() == Delimiter::Parenthesis {
                break;
            }
        }
        if let TokenTree::Literal(_) = &tokens[i] {
            break;
        }
        return_type_tokens.push(tokens[i].clone());
        i += 1;
    }
    let return_type = if return_type_tokens.is_empty() {
        None
    } else {
        let rt: TokenStream2 = return_type_tokens.into_iter().collect();
        Some(rt)
    };

    // Check for `where` clause
    let mut where_clause = None;
    if i < tokens.len() && is_ident(&tokens[i], "where") {
        i += 1;
        if i < tokens.len() {
            if let TokenTree::Group(g) = &tokens[i] {
                if g.delimiter() == Delimiter::Parenthesis {
                    let inner = g.stream();
                    where_clause = Some(inner);
                    i += 1;
                }
            }
        }
    }

    // Remaining tokens: body expressions (parenthesized groups) or bare expr
    let mut body_items = Vec::new();
    let mut has_body = false;
    while i < tokens.len() {
        has_body = true;
        if let TokenTree::Group(g) = &tokens[i] {
            if g.delimiter() == Delimiter::Parenthesis {
                let inner: Vec<TokenTree> = g.stream().into_iter().collect();
                body_items.push(eval_lisp_expr(&inner));
                i += 1;
                continue;
            }
        }
        // Bare expression token
        let tok = tokens[i].clone();
        body_items.push(quote! { #tok });
        i += 1;
    }

    let gen = match &generics {
        Some(g) => quote! { <#g> },
        None => quote! {},
    };
    let ret = match &return_type {
        Some(r) => quote! { -> #r },
        None => quote! {},
    };
    let where_cl = match &where_clause {
        Some(w) => quote! { where #w },
        None => quote! {},
    };

    if has_body {
        Ok(quote! {
            #attrs_ts
            #quals_ts fn #fn_name #gen (#(#params),*) #ret #where_cl {
                #(#body_items);*
            }
        })
    } else {
        // Declaration only (no body) — emit semicolon
        Ok(quote! {
            #attrs_ts
            #quals_ts fn #fn_name #gen (#(#params),*) #ret #where_cl;
        })
    }
}

// ─── lisp_assign! ───────────────────────────────────────────────────────────
//
// Usage: lisp_assign!(= lhs_tokens... rhs)
// or:   lisp_assign!(+= lhs_tokens... rhs)
//
// The last token tree is the RHS (possibly a parenthesized group for lisp recursion).
// Everything between the operator and the last token is the LHS (emitted verbatim).
#[proc_macro]
pub fn lisp_assign(input: TokenStream) -> TokenStream {
    let tokens: Vec<TokenTree> = TokenStream2::from(input).into_iter().collect();
    if tokens.len() < 3 {
        return syn::Error::new(Span::call_site(), "lisp_assign! requires at least an operator, LHS, and RHS")
            .to_compile_error()
            .into();
    }

    // Parse the operator (first 1 or 2 punctuation tokens)
    let (op_tokens, rest) = parse_assign_op(&tokens);
    if op_tokens.is_empty() || rest.is_empty() {
        return syn::Error::new(Span::call_site(), "lisp_assign! invalid operator or missing operands")
            .to_compile_error()
            .into();
    }

    let op_ts: TokenStream2 = op_tokens.into_iter().collect();

    // Split rest into LHS (all but last) and RHS (last token)
    if rest.len() < 2 {
        return syn::Error::new(Span::call_site(), "lisp_assign! requires both LHS and RHS")
            .to_compile_error()
            .into();
    }

    let lhs_tokens = &rest[..rest.len() - 1];
    let rhs_token = &rest[rest.len() - 1];

    let lhs_ts: TokenStream2 = lhs_tokens.iter().cloned().collect();

    // If RHS is a parenthesized group, recurse through lisp!
    let rhs_ts = match rhs_token {
        TokenTree::Group(g) if g.delimiter() == Delimiter::Parenthesis => {
            let inner = g.stream();
            quote! { ::lisp::lisp!(#inner) }
        }
        other => {
            let other = other.clone();
            quote! { #other }
        }
    };

    let result = quote! {
        #lhs_ts #op_ts #rhs_ts;
    };
    result.into()
}

fn parse_assign_op(tokens: &[TokenTree]) -> (Vec<TokenTree>, &[TokenTree]) {
    // Try to parse compound operators like +=, -=, *=, /=, %=, &=, |=, ^=, <<=, >>=
    // or simple =
    let mut i = 0;
    let mut op = Vec::new();

    if i >= tokens.len() {
        return (op, tokens);
    }

    match &tokens[i] {
        TokenTree::Punct(p) => {
            let ch = p.as_char();
            match ch {
                '=' => {
                    op.push(tokens[i].clone());
                    return (op, &tokens[i + 1..]);
                }
                '+' | '-' | '*' | '/' | '%' | '&' | '|' | '^' => {
                    op.push(tokens[i].clone());
                    i += 1;
                    if i < tokens.len() {
                        if let TokenTree::Punct(p2) = &tokens[i] {
                            if p2.as_char() == '=' {
                                op.push(tokens[i].clone());
                                return (op, &tokens[i + 1..]);
                            }
                        }
                    }
                    // Not a compound op, shouldn't happen
                    return (vec![], tokens);
                }
                '<' => {
                    // Could be <<= 
                    op.push(tokens[i].clone());
                    i += 1;
                    if i < tokens.len() {
                        if let TokenTree::Punct(p2) = &tokens[i] {
                            if p2.as_char() == '<' {
                                op.push(tokens[i].clone());
                                i += 1;
                                if i < tokens.len() {
                                    if let TokenTree::Punct(p3) = &tokens[i] {
                                        if p3.as_char() == '=' {
                                            op.push(tokens[i].clone());
                                            return (op, &tokens[i + 1..]);
                                        }
                                    }
                                }
                            }
                        }
                    }
                    return (vec![], tokens);
                }
                '>' => {
                    // Could be >>=
                    op.push(tokens[i].clone());
                    i += 1;
                    if i < tokens.len() {
                        if let TokenTree::Punct(p2) = &tokens[i] {
                            if p2.as_char() == '>' {
                                op.push(tokens[i].clone());
                                i += 1;
                                if i < tokens.len() {
                                    if let TokenTree::Punct(p3) = &tokens[i] {
                                        if p3.as_char() == '=' {
                                            op.push(tokens[i].clone());
                                            return (op, &tokens[i + 1..]);
                                        }
                                    }
                                }
                            }
                        }
                    }
                    return (vec![], tokens);
                }
                _ => return (vec![], tokens),
            }
        }
        _ => return (vec![], tokens),
    }
}

// ─── lisp_impl! ─────────────────────────────────────────────────────────────
//
// Usage: lisp_impl!(GenericParams Type (method)...)
//        lisp_impl!(GenericParams Trait for Type (method)...)
//        lisp_impl!(Type (method)...)
#[proc_macro]
pub fn lisp_impl(input: TokenStream) -> TokenStream {
    let tokens: Vec<TokenTree> = TokenStream2::from(input).into_iter().collect();
    let result = parse_impl(&tokens);
    match result {
        Ok(ts) => ts.into(),
        Err(e) => e.to_compile_error().into(),
    }
}

fn parse_impl(tokens: &[TokenTree]) -> syn::Result<TokenStream2> {
    let mut i = 0;

    // 1. Check for <...> generic params
    let mut generics = None;
    if i < tokens.len() && is_punct(&tokens[i], '<') {
        let (angle_contents, rest) = consume_angle_brackets(&tokens[i..]);
        let angle_ts: TokenStream2 = angle_contents.into_iter().collect();
        generics = Some(angle_ts);
        i = tokens.len() - rest.len();
    }

    // 2. Consume the first type/trait path
    let (first_path, rest) = consume_type_path(&tokens[i..]);
    if first_path.is_empty() {
        return Err(syn::Error::new(Span::call_site(), "lisp_impl!: expected type or trait name"));
    }
    let first_path_ts: TokenStream2 = first_path.into_iter().collect();
    i = tokens.len() - rest.len();

    // 3. Check for `for` keyword
    let (trait_path, impl_type) = if i < tokens.len() && is_ident(&tokens[i], "for") {
        i += 1; // skip `for`
        let (type_path, rest2) = consume_type_path(&tokens[i..]);
        if type_path.is_empty() {
            return Err(syn::Error::new(Span::call_site(), "lisp_impl!: expected type after 'for'"));
        }
        let type_path_ts: TokenStream2 = type_path.into_iter().collect();
        i = tokens.len() - rest2.len();
        (Some(first_path_ts), type_path_ts)
    } else {
        (None, first_path_ts)
    };

    // 4. Check for `where` clause
    let mut where_clause = None;
    if i < tokens.len() && is_ident(&tokens[i], "where") {
        i += 1; // skip `where`
        // Consume tokens until we hit a parenthesized group (which starts method bodies)
        let mut where_tokens = Vec::new();
        // The where clause may be in a parenthesized group
        if i < tokens.len() {
            if let TokenTree::Group(g) = &tokens[i] {
                if g.delimiter() == Delimiter::Parenthesis {
                    // where (T: Clone + Debug)
                    where_tokens.extend(g.stream().into_iter());
                    i += 1;
                }
            }
        }
        if !where_tokens.is_empty() {
            let where_ts: TokenStream2 = where_tokens.into_iter().collect();
            where_clause = Some(where_ts);
        }
    }

    // 5. Remaining tokens should be parenthesized groups containing method/type definitions
    let mut body_items = Vec::new();
    while i < tokens.len() {
        if let TokenTree::Group(g) = &tokens[i] {
            if g.delimiter() == Delimiter::Parenthesis {
                let inner: Vec<TokenTree> = g.stream().into_iter().collect();
                let item = parse_impl_body_item(&inner)?;
                body_items.push(item);
            }
        }
        i += 1;
    }

    // Build the impl block
    let gen = match &generics {
        Some(g) => quote! { <#g> },
        None => quote! {},
    };
    let where_cl = match &where_clause {
        Some(w) => quote! { where #w },
        None => quote! {},
    };
    let trait_for = match &trait_path {
        Some(t) => quote! { #t for },
        None => quote! {},
    };

    Ok(quote! {
        impl #gen #trait_for #impl_type #where_cl {
            #(#body_items)*
        }
    })
}

// ─── lisp_trait! ────────────────────────────────────────────────────────────
#[proc_macro]
pub fn lisp_trait(input: TokenStream) -> TokenStream {
    let tokens: Vec<TokenTree> = TokenStream2::from(input).into_iter().collect();
    let result = parse_trait(&tokens);
    match result {
        Ok(ts) => ts.into(),
        Err(e) => e.to_compile_error().into(),
    }
}

fn parse_trait(tokens: &[TokenTree]) -> syn::Result<TokenStream2> {
    let mut i = 0;

    // 1. Check for pub
    let is_pub = i < tokens.len() && is_ident(&tokens[i], "pub");
    if is_pub { i += 1; }

    // 2. Consume trait name
    let trait_name = if i < tokens.len() {
        if let TokenTree::Ident(id) = &tokens[i] {
            let id = id.clone();
            i += 1;
            id
        } else {
            return Err(syn::Error::new(Span::call_site(), "lisp_trait!: expected trait name"));
        }
    } else {
        return Err(syn::Error::new(Span::call_site(), "lisp_trait!: expected trait name"));
    };

    // 3. Check for <...> generic params
    let mut generics = None;
    if i < tokens.len() && is_punct(&tokens[i], '<') {
        let (angle_contents, rest) = consume_angle_brackets(&tokens[i..]);
        let angle_ts: TokenStream2 = angle_contents.into_iter().collect();
        generics = Some(angle_ts);
        i = tokens.len() - rest.len();
    }

    // 4. Check for `:` followed by supertraits
    let mut supertraits = None;
    if i < tokens.len() && is_punct(&tokens[i], ':') {
        i += 1; // skip `:`
        let mut supertrait_tokens = Vec::new();
        // Consume tokens until we hit a parenthesized group or `where`
        while i < tokens.len() {
            if let TokenTree::Group(g) = &tokens[i] {
                if g.delimiter() == Delimiter::Parenthesis {
                    break;
                }
            }
            if is_ident(&tokens[i], "where") {
                break;
            }
            supertrait_tokens.push(tokens[i].clone());
            i += 1;
        }
        if !supertrait_tokens.is_empty() {
            let st_ts: TokenStream2 = supertrait_tokens.into_iter().collect();
            supertraits = Some(st_ts);
        }
    }

    // 5. Check for `where` clause
    let mut where_clause = None;
    if i < tokens.len() && is_ident(&tokens[i], "where") {
        i += 1; // skip `where`
        if i < tokens.len() {
            if let TokenTree::Group(g) = &tokens[i] {
                if g.delimiter() == Delimiter::Parenthesis {
                    let inner = g.stream();
                    where_clause = Some(inner);
                    i += 1;
                }
            }
        }
    }

    // 6. Remaining tokens are method/type declarations in (...) groups
    let mut body_items = Vec::new();
    while i < tokens.len() {
        if let TokenTree::Group(g) = &tokens[i] {
            if g.delimiter() == Delimiter::Parenthesis {
                let inner: Vec<TokenTree> = g.stream().into_iter().collect();
                let item = parse_impl_body_item(&inner)?;
                body_items.push(item);
            }
        }
        i += 1;
    }

    let pub_kw = if is_pub { quote! { pub } } else { quote! {} };
    let gen = match &generics {
        Some(g) => quote! { <#g> },
        None => quote! {},
    };
    let super_cl = match &supertraits {
        Some(s) => quote! { : #s },
        None => quote! {},
    };
    let where_cl = match &where_clause {
        Some(w) => quote! { where #w },
        None => quote! {},
    };

    Ok(quote! {
        #pub_kw trait #trait_name #gen #super_cl #where_cl {
            #(#body_items)*
        }
    })
}

// ─── lisp_enum! ─────────────────────────────────────────────────────────────
#[proc_macro]
pub fn lisp_enum(input: TokenStream) -> TokenStream {
    let tokens: Vec<TokenTree> = TokenStream2::from(input).into_iter().collect();
    let result = parse_enum(&tokens);
    match result {
        Ok(ts) => ts.into(),
        Err(e) => e.to_compile_error().into(),
    }
}

fn parse_enum(tokens: &[TokenTree]) -> syn::Result<TokenStream2> {
    let mut i = 0;

    // 1. Check for attributes and pub
    let mut attrs = Vec::new();
    while i < tokens.len() {
        if let TokenTree::Punct(p) = &tokens[i] {
            if p.as_char() == '#' {
                attrs.push(tokens[i].clone());
                i += 1;
                if i < tokens.len() {
                    attrs.push(tokens[i].clone());
                    i += 1;
                }
                continue;
            }
        }
        break;
    }
    let attrs_ts: TokenStream2 = attrs.into_iter().collect();

    let is_pub = i < tokens.len() && is_ident(&tokens[i], "pub");
    if is_pub { i += 1; }

    // Skip `enum` keyword if present
    if i < tokens.len() && is_ident(&tokens[i], "enum") {
        i += 1;
    }

    // 2. Consume enum name
    let enum_name = if i < tokens.len() {
        if let TokenTree::Ident(id) = &tokens[i] {
            let id = id.clone();
            i += 1;
            id
        } else {
            return Err(syn::Error::new(Span::call_site(), "lisp_enum!: expected enum name"));
        }
    } else {
        return Err(syn::Error::new(Span::call_site(), "lisp_enum!: expected enum name"));
    };

    // 3. Check for <...> generic params
    let mut generics = None;
    if i < tokens.len() && is_punct(&tokens[i], '<') {
        let (angle_contents, rest) = consume_angle_brackets(&tokens[i..]);
        let angle_ts: TokenStream2 = angle_contents.into_iter().collect();
        generics = Some(angle_ts);
        i = tokens.len() - rest.len();
    }

    // 4. Parse variants - each is a (...) group
    let mut variants = Vec::new();
    while i < tokens.len() {
        if let TokenTree::Group(g) = &tokens[i] {
            if g.delimiter() == Delimiter::Parenthesis {
                let variant_ts = parse_enum_variant(g.stream())?;
                variants.push(variant_ts);
            }
        }
        i += 1;
    }

    let pub_kw = if is_pub { quote! { pub } } else { quote! {} };
    let gen = match &generics {
        Some(g) => quote! { <#g> },
        None => quote! {},
    };

    Ok(quote! {
        #attrs_ts
        #pub_kw enum #enum_name #gen {
            #(#variants),*
        }
    })
}

fn parse_enum_variant(stream: TokenStream2) -> syn::Result<TokenStream2> {
    let tokens: Vec<TokenTree> = stream.into_iter().collect();
    if tokens.is_empty() {
        return Err(syn::Error::new(Span::call_site(), "empty enum variant"));
    }

    // First token should be the variant name
    let variant_name = if let TokenTree::Ident(id) = &tokens[0] {
        id.clone()
    } else {
        return Err(syn::Error::new(Span::call_site(), "expected variant name"));
    };

    if tokens.len() == 1 {
        // Unit variant
        return Ok(quote! { #variant_name });
    }

    // Check if the next token is a parenthesized group containing (field Type) pairs -> struct variant
    if tokens.len() == 2 {
        if let TokenTree::Group(g) = &tokens[1] {
            if g.delimiter() == Delimiter::Parenthesis {
                // Check if contents look like (field Type) pairs
                let inner_tokens: Vec<TokenTree> = g.stream().into_iter().collect();
                if !inner_tokens.is_empty() {
                    if let TokenTree::Group(inner_g) = &inner_tokens[0] {
                        if inner_g.delimiter() == Delimiter::Parenthesis {
                            // It's a struct variant with (field Type) pairs
                            let fields = parse_struct_variant_fields(g.stream())?;
                            return Ok(quote! { #variant_name { #fields } });
                        }
                    }
                }
            }
        }
    }

    // Otherwise it's a tuple variant - remaining tokens are types
    let type_tokens: Vec<&TokenTree> = tokens[1..].iter().collect();
    let types_ts: TokenStream2 = type_tokens.into_iter().cloned().collect();
    Ok(quote! { #variant_name(#types_ts) })
}

fn parse_struct_variant_fields(stream: TokenStream2) -> syn::Result<TokenStream2> {
    let tokens: Vec<TokenTree> = stream.into_iter().collect();
    let mut fields = Vec::new();

    for tt in &tokens {
        if let TokenTree::Group(g) = tt {
            if g.delimiter() == Delimiter::Parenthesis {
                let inner: Vec<TokenTree> = g.stream().into_iter().collect();
                if inner.len() >= 2 {
                    if let TokenTree::Ident(field_name) = &inner[0] {
                        let field_type: TokenStream2 = inner[1..].iter().cloned().collect();
                        fields.push(quote! { #field_name: #field_type });
                    }
                }
            }
        }
    }

    let result = if fields.len() == 1 {
        let f = &fields[0];
        quote! { #f }
    } else {
        quote! { #(#fields),* }
    };
    Ok(result)
}

// ─── lisp_struct! ───────────────────────────────────────────────────────────
#[proc_macro]
pub fn lisp_struct(input: TokenStream) -> TokenStream {
    let tokens: Vec<TokenTree> = TokenStream2::from(input).into_iter().collect();
    let result = parse_struct(&tokens);
    match result {
        Ok(ts) => ts.into(),
        Err(e) => e.to_compile_error().into(),
    }
}

fn parse_struct(tokens: &[TokenTree]) -> syn::Result<TokenStream2> {
    let mut i = 0;

    // 1. Collect attributes
    let mut attrs = Vec::new();
    while i < tokens.len() {
        if let TokenTree::Punct(p) = &tokens[i] {
            if p.as_char() == '#' {
                attrs.push(tokens[i].clone());
                i += 1;
                if i < tokens.len() {
                    attrs.push(tokens[i].clone());
                    i += 1;
                }
                continue;
            }
        }
        break;
    }
    let attrs_ts: TokenStream2 = attrs.into_iter().collect();

    // 2. Check for pub
    let is_pub = i < tokens.len() && is_ident(&tokens[i], "pub");
    if is_pub { i += 1; }

    // Skip `struct` keyword
    if i < tokens.len() && is_ident(&tokens[i], "struct") {
        i += 1;
    }

    // 3. Consume struct name
    let struct_name = if i < tokens.len() {
        if let TokenTree::Ident(id) = &tokens[i] {
            let id = id.clone();
            i += 1;
            id
        } else {
            return Err(syn::Error::new(Span::call_site(), "lisp_struct!: expected struct name"));
        }
    } else {
        return Err(syn::Error::new(Span::call_site(), "lisp_struct!: expected struct name"));
    };

    // 4. Consume <...> generic params (mandatory for this proc macro path)
    let mut generics = None;
    if i < tokens.len() && is_punct(&tokens[i], '<') {
        let (angle_contents, rest) = consume_angle_brackets(&tokens[i..]);
        let angle_ts: TokenStream2 = angle_contents.into_iter().collect();
        generics = Some(angle_ts);
        i = tokens.len() - rest.len();
    }

    // 5. Check for where clause
    let mut where_clause = None;
    if i < tokens.len() && is_ident(&tokens[i], "where") {
        i += 1;
        if i < tokens.len() {
            if let TokenTree::Group(g) = &tokens[i] {
                if g.delimiter() == Delimiter::Parenthesis {
                    let inner = g.stream();
                    where_clause = Some(inner);
                    i += 1;
                }
            }
        }
    }

    let pub_kw = if is_pub { quote! { pub } } else { quote! {} };
    let gen = match &generics {
        Some(g) => quote! { <#g> },
        None => quote! {},
    };
    let where_cl = match &where_clause {
        Some(w) => quote! { where #w },
        None => quote! {},
    };

    // 6. Parse fields - remaining tokens should be a parenthesized group
    if i >= tokens.len() {
        // Unit struct with generics
        return Ok(quote! { #attrs_ts #pub_kw struct #struct_name #gen #where_cl; });
    }

    // Check if we have field groups
    if let TokenTree::Group(g) = &tokens[i] {
        if g.delimiter() == Delimiter::Parenthesis {
            let inner: Vec<TokenTree> = g.stream().into_iter().collect();

            // Check if it looks like named fields: ((name Type) (name Type) ...)
            if !inner.is_empty() {
                if let TokenTree::Group(_) = &inner[0] {
                    // Named struct fields
                    let mut fields = Vec::new();
                    // Check for `pub` keyword first in the group
                    let mut is_pub_fields = false;
                    let mut field_start = 0;
                    if is_ident(&inner[0], "pub") {
                        is_pub_fields = true;
                        field_start = 1;
                    }

                    for tt in &inner[field_start..] {
                        if let TokenTree::Group(fg) = tt {
                            if fg.delimiter() == Delimiter::Parenthesis {
                                let field_tokens: Vec<TokenTree> = fg.stream().into_iter().collect();
                                if field_tokens.len() >= 2 {
                                    if let TokenTree::Ident(field_name) = &field_tokens[0] {
                                        let field_type: TokenStream2 = field_tokens[1..].iter().cloned().collect();
                                        if is_pub_fields {
                                            fields.push(quote! { pub #field_name: #field_type });
                                        } else {
                                            fields.push(quote! { #field_name: #field_type });
                                        }
                                    }
                                }
                            }
                        }
                    }
                    return Ok(quote! {
                        #attrs_ts
                        #pub_kw struct #struct_name #gen #where_cl {
                            #(#fields),*
                        }
                    });
                } else {
                    // Tuple struct
                    let types: TokenStream2 = inner.into_iter().collect();
                    return Ok(quote! {
                        #attrs_ts
                        #pub_kw struct #struct_name #gen #where_cl ( #types );
                    });
                }
            }
        }
    }

    // Fallback: unit struct
    Ok(quote! { #attrs_ts #pub_kw struct #struct_name #gen #where_cl; })
}

// ─── lisp_fn! ───────────────────────────────────────────────────────────────
#[proc_macro]
pub fn lisp_fn(input: TokenStream) -> TokenStream {
    let tokens: Vec<TokenTree> = TokenStream2::from(input).into_iter().collect();
    let result = parse_fn(&tokens);
    match result {
        Ok(ts) => ts.into(),
        Err(e) => e.to_compile_error().into(),
    }
}

fn parse_fn(tokens: &[TokenTree]) -> syn::Result<TokenStream2> {
    let mut i = 0;

    // 1. Collect attributes
    let mut attrs = Vec::new();
    while i < tokens.len() {
        if let TokenTree::Punct(p) = &tokens[i] {
            if p.as_char() == '#' {
                attrs.push(tokens[i].clone());
                i += 1;
                if i < tokens.len() {
                    attrs.push(tokens[i].clone());
                    i += 1;
                }
                continue;
            }
        }
        break;
    }
    let attrs_ts: TokenStream2 = attrs.into_iter().collect();

    // 2. Collect qualifiers: pub, const, unsafe, async, extern "C"
    let mut qualifiers = Vec::new();
    loop {
        if i >= tokens.len() { break; }
        match &tokens[i] {
            TokenTree::Ident(id) if *id == "pub" || *id == "const" || *id == "unsafe" || *id == "async" => {
                qualifiers.push(tokens[i].clone());
                i += 1;
            }
            TokenTree::Ident(id) if *id == "extern" => {
                qualifiers.push(tokens[i].clone());
                i += 1;
                // Consume ABI string if present
                if i < tokens.len() {
                    if let TokenTree::Literal(_) = &tokens[i] {
                        qualifiers.push(tokens[i].clone());
                        i += 1;
                    }
                }
            }
            _ => break,
        }
    }
    let quals_ts: TokenStream2 = qualifiers.into_iter().collect();

    // 3. Skip `fn` keyword
    if i < tokens.len() && is_ident(&tokens[i], "fn") {
        i += 1;
    }

    // 4. Consume function name
    let fn_name = if i < tokens.len() {
        if let TokenTree::Ident(id) = &tokens[i] {
            let id = id.clone();
            i += 1;
            id
        } else {
            return Err(syn::Error::new(Span::call_site(), "lisp_fn!: expected function name"));
        }
    } else {
        return Err(syn::Error::new(Span::call_site(), "lisp_fn!: expected function name"));
    };

    // 5. Consume <...> generic params
    let mut generics = None;
    if i < tokens.len() && is_punct(&tokens[i], '<') {
        let (angle_contents, rest) = consume_angle_brackets(&tokens[i..]);
        let angle_ts: TokenStream2 = angle_contents.into_iter().collect();
        generics = Some(angle_ts);
        i = tokens.len() - rest.len();
    }

    // 6. Consume parameter list: a parenthesized group containing (name Type) pairs
    let mut params = Vec::new();
    if i < tokens.len() {
        if let TokenTree::Group(g) = &tokens[i] {
            if g.delimiter() == Delimiter::Parenthesis {
                let param_tokens: Vec<TokenTree> = g.stream().into_iter().collect();
                for tt in &param_tokens {
                    if let TokenTree::Group(pg) = tt {
                        if pg.delimiter() == Delimiter::Parenthesis {
                            let inner: Vec<TokenTree> = pg.stream().into_iter().collect();
                            if inner.len() >= 2 {
                                let param_name = &inner[0];
                                let param_type: TokenStream2 = inner[1..].iter().cloned().collect();
                                params.push(quote! { #param_name: #param_type });
                            }
                        }
                    }
                }
                i += 1;
            }
        }
    }

    // 7. Check for return type (next token that's not a group, not `where`, not a paren group for body)
    let mut return_type = None;
    // The return type is a single token tree that comes before the where clause and body groups
    // It could be an ident like `T`, or a path, or missing
    if i < tokens.len() {
        // Check if this token could be a return type
        match &tokens[i] {
            TokenTree::Ident(id) if *id != "where" => {
                // Could be a return type - peek ahead to see if next is a body group or `where`
                // Collect return type tokens until we hit `where` or a parenthesized group that starts a body
                let mut ret_tokens = Vec::new();
                while i < tokens.len() {
                    if is_ident(&tokens[i], "where") { break; }
                    if let TokenTree::Group(g) = &tokens[i] {
                        if g.delimiter() == Delimiter::Parenthesis {
                            // This could be a body - check if it looks like a lisp expression
                            break;
                        }
                    }
                    ret_tokens.push(tokens[i].clone());
                    i += 1;
                }
                if !ret_tokens.is_empty() {
                    let ret_ts: TokenStream2 = ret_tokens.into_iter().collect();
                    return_type = Some(ret_ts);
                }
            }
            // Handle path-based return types like core::fmt::Result
            TokenTree::Punct(_) => {
                // Could be a complex return type path; skip for now
            }
            _ => {}
        }
    }

    // 8. Check for `where` clause
    let mut where_clause = None;
    if i < tokens.len() && is_ident(&tokens[i], "where") {
        i += 1;
        if i < tokens.len() {
            if let TokenTree::Group(g) = &tokens[i] {
                if g.delimiter() == Delimiter::Parenthesis {
                    let inner = g.stream();
                    where_clause = Some(inner);
                    i += 1;
                }
            }
        }
    }

    // 9. Remaining parenthesized groups are body expressions
    let mut body_items = Vec::new();
    while i < tokens.len() {
        if let TokenTree::Group(g) = &tokens[i] {
            if g.delimiter() == Delimiter::Parenthesis {
                let inner = g.stream();
                body_items.push(quote! { ::lisp::lisp! { #inner } });
            }
        }
        i += 1;
    }

    let gen = match &generics {
        Some(g) => quote! { <#g> },
        None => quote! {},
    };
    let ret = match &return_type {
        Some(r) => quote! { -> #r },
        None => quote! {},
    };
    let where_cl = match &where_clause {
        Some(w) => quote! { where #w },
        None => quote! {},
    };

    Ok(quote! {
        #attrs_ts
        #quals_ts fn #fn_name #gen (#(#params),*) #ret #where_cl {
            #(#body_items);*
        }
    })
}
