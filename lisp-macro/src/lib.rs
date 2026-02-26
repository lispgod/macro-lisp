use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use proc_macro2::{Delimiter, TokenTree, Spacing, Punct, Span};
use proc_macro_error2::{abort, proc_macro_error};
use quote::{quote, quote_spanned, ToTokens};

// ─── Debug: pretty-print generated code when `debug-expansion` feature is enabled ───
#[cfg(feature = "debug-expansion")]
fn debug_expansion(label: &str, output: &TokenStream2) {
    if let Ok(file) = syn::parse_file(&output.to_string()) {
        eprintln!("=== {} expansion ===\n{}", label, prettyplease::unparse(&file));
    }
}

#[cfg(not(feature = "debug-expansion"))]
fn debug_expansion(_label: &str, _output: &TokenStream2) {}

// ─── Helper: flatten invisible delimiter groups ─────────────────────────────
// macro_rules! wraps captured fragments ($vis:vis, $expr:expr, etc.) in
// Group { delimiter: None } ("invisible groups"). When these are forwarded
// to a proc macro, the proc macro sees the wrapper instead of the raw tokens.
// This helper flattens top-level invisible groups so parsing sees raw tokens.
fn flatten_none_delim(tokens: Vec<TokenTree>) -> Vec<TokenTree> {
    let mut result = Vec::new();
    for tt in tokens {
        if let TokenTree::Group(ref g) = tt {
            if g.delimiter() == Delimiter::None {
                result.extend(flatten_none_delim(g.stream().into_iter().collect()));
                continue;
            }
        }
        result.push(tt);
    }
    result
}

// ─── Helper: parse a space-separated list of types from a token stream ───
// In the lisp DSL, tuple struct fields are written as `(Type1 Type2 ...)` without commas.
// This uses syn to greedily parse each complete type.
fn parse_type_list(stream: TokenStream2) -> syn::Result<Vec<syn::Type>> {
    struct TypeList(Vec<syn::Type>);
    impl syn::parse::Parse for TypeList {
        fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
            let mut types = Vec::new();
            while !input.is_empty() {
                types.push(input.parse::<syn::Type>()?);
            }
            Ok(TypeList(types))
        }
    }
    let list = syn::parse2::<TypeList>(stream)?;
    Ok(list.0)
}

// ─── Helper: check if a token tree is a specific punctuation character ───
fn is_punct(tt: &TokenTree, ch: char) -> bool {
    matches!(tt, TokenTree::Punct(p) if p.as_char() == ch)
}

// ─── Helper: check if a token tree is a specific ident ───
fn is_ident(tt: &TokenTree, name: &str) -> bool {
    matches!(tt, TokenTree::Ident(i) if *i == name)
}

// ─── Helper: check if a token tree is a literal (or a literal wrapped in an invisible group) ───
// macro_rules! fragment captures like `$abi:literal` wrap the literal in a Group { delimiter: None }.
fn is_literal_like(tt: &TokenTree) -> bool {
    match tt {
        TokenTree::Literal(_) => true,
        TokenTree::Group(g) if g.delimiter() == Delimiter::None => {
            g.stream().into_iter().any(|t| matches!(t, TokenTree::Literal(_)))
        }
        _ => false,
    }
}

// ─── Helper: validate a token stream as a Rust type using syn::Type ───
// Returns the validated type tokens, falling back to raw tokens if syn can't parse them.
fn validate_type(tokens: TokenStream2) -> TokenStream2 {
    match syn::parse2::<syn::Type>(tokens.clone()) {
        Ok(ty) => quote! { #ty },
        Err(_) => {
            // Fall back to raw tokens — some DSL type forms may not parse as syn::Type
            // but are still valid in context (e.g., when macro_rules expands them later).
            tokens
        }
    }
}

// ─── Helper: validate a token stream as a Rust pattern using syn::Pat ───
fn validate_pattern(tokens: TokenStream2) -> TokenStream2 {
    struct PatWrapper(syn::Pat);
    impl syn::parse::Parse for PatWrapper {
        fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
            syn::Pat::parse_multi(input).map(PatWrapper)
        }
    }
    match syn::parse2::<PatWrapper>(tokens.clone()) {
        Ok(PatWrapper(pat)) => quote! { #pat },
        Err(_) => tokens,
    }
}

// ─── Helper: validate angle bracket tokens as syn::Generics ───
fn validate_generics(tokens: &TokenStream2) -> TokenStream2 {
    let wrapped = quote! { <#tokens> };
    match syn::parse2::<syn::Generics>(wrapped) {
        Ok(_) => tokens.clone(),
        Err(_) => tokens.clone(),
    }
}

// ─── Helper: parse a self parameter from tokens inside a parameter group ───
// Returns Some(token_stream) if it's a self parameter form, None otherwise.
// Only handles shorthand forms where self doesn't have a type annotation;
// (self Type) falls through to regular parameter parsing to preserve token spans.
fn parse_self_param(inner: &[TokenTree]) -> Option<TokenStream2> {
    if inner.is_empty() {
        return None;
    }
    // (self) → self
    if inner.len() == 1 && is_ident(&inner[0], "self") {
        let s = &inner[0];
        return Some(quote! { #s });
    }
    // (&self) → &self
    if inner.len() == 2 && is_punct(&inner[0], '&') && is_ident(&inner[1], "self") {
        let amp = &inner[0];
        let s = &inner[1];
        return Some(quote! { #amp #s });
    }
    // (&mut self) → &mut self
    if inner.len() == 3 && is_punct(&inner[0], '&') && is_ident(&inner[1], "mut") && is_ident(&inner[2], "self") {
        let amp = &inner[0];
        let m = &inner[1];
        let s = &inner[2];
        return Some(quote! { #amp #m #s });
    }
    // (mut self) → mut self
    if inner.len() == 2 && is_ident(&inner[0], "mut") && is_ident(&inner[1], "self") {
        let m = &inner[0];
        let s = &inner[1];
        return Some(quote! { #m #s });
    }
    None
}

// ─── Helper: parse visibility modifier using syn::Visibility ───
// Returns (visibility_tokens, tokens_consumed)
fn parse_visibility(tokens: &[TokenTree]) -> (TokenStream2, usize) {
    if tokens.is_empty() {
        return (quote! {}, 0);
    }
    // Try to parse a syn::Visibility from the token prefix.
    // We attempt to consume `pub`, `pub(crate)`, `pub(super)`, `pub(in path)`.
    if !is_ident(&tokens[0], "pub") {
        return (quote! {}, 0);
    }
    // Determine how many tokens the visibility consumes
    let consumed = if tokens.len() >= 2 {
        if let TokenTree::Group(g) = &tokens[1] {
            if g.delimiter() == Delimiter::Parenthesis {
                2 // pub(...)
            } else {
                1 // just pub
            }
        } else {
            1 // just pub
        }
    } else {
        1 // just pub
    };
    let vis_tokens: TokenStream2 = tokens[..consumed].iter().cloned().collect();
    // Validate with syn::Visibility
    match syn::parse2::<syn::Visibility>(vis_tokens.clone()) {
        Ok(vis) => (quote! { #vis }, consumed),
        Err(e) => abort!(tokens[0].span(), "invalid visibility: {}", e),
    }
}

// ─── Shared parsing helpers ──────────────────────────────────────────────────
// These extract common patterns used across parse_struct, parse_enum,
// parse_trait, parse_impl, and parse_fn_signature, eliminating duplication.

/// Collect `#[...]` attribute tokens, advancing `*i`.
fn parse_attributes(tokens: &[TokenTree], i: &mut usize) -> TokenStream2 {
    let mut attrs = Vec::new();
    while *i < tokens.len() {
        if let TokenTree::Punct(p) = &tokens[*i] {
            if p.as_char() == '#' {
                attrs.push(tokens[*i].clone());
                *i += 1;
                if *i < tokens.len() {
                    attrs.push(tokens[*i].clone());
                    *i += 1;
                }
                continue;
            }
        }
        break;
    }
    attrs.into_iter().collect()
}

/// Parse `<...>` generic parameters, advancing `*i`. Returns None if no `<` found.
/// Validates the collected tokens with `syn::Generics`.
fn parse_generics_params(tokens: &[TokenTree], i: &mut usize) -> Option<TokenStream2> {
    if *i < tokens.len() && is_punct(&tokens[*i], '<') {
        let (angle_contents, rest) = consume_angle_brackets(&tokens[*i..]);
        let angle_ts: TokenStream2 = angle_contents.into_iter().collect();
        *i = tokens.len() - rest.len();
        Some(validate_generics(&angle_ts))
    } else {
        None
    }
}

/// Parse `where (clause)`, advancing `*i`. Returns None if no `where` found.
fn parse_where_clause(tokens: &[TokenTree], i: &mut usize) -> Option<TokenStream2> {
    if *i < tokens.len() && is_ident(&tokens[*i], "where") {
        *i += 1;
        if *i < tokens.len() {
            if let TokenTree::Group(g) = &tokens[*i] {
                if g.delimiter() == Delimiter::Parenthesis {
                    *i += 1;
                    return Some(g.stream());
                }
            }
        }
    }
    None
}

/// Emit `<G>` from an optional generics token stream.
fn emit_generics(gen: &Option<TokenStream2>) -> TokenStream2 {
    match gen {
        Some(g) => quote! { <#g> },
        None => quote! {},
    }
}

/// Emit `-> R` from an optional return type token stream.
fn emit_return_type(ret: &Option<TokenStream2>) -> TokenStream2 {
    match ret {
        Some(r) => quote! { -> #r },
        None => quote! {},
    }
}

/// Emit `where W` from an optional where clause token stream.
fn emit_where_clause(wc: &Option<TokenStream2>) -> TokenStream2 {
    match wc {
        Some(w) => quote! { where #w },
        None => quote! {},
    }
}

/// Collect body items from remaining tokens (parenthesized groups → eval_lisp_expr, bare → quoted).
fn parse_body_items(tokens: &[TokenTree], start: usize) -> Vec<LispOutput> {
    let mut items = Vec::new();
    let mut i = start;
    while i < tokens.len() {
        if let TokenTree::Group(g) = &tokens[i] {
            if g.delimiter() == Delimiter::Parenthesis {
                let inner: Vec<TokenTree> = g.stream().into_iter().collect();
                items.push(eval_lisp_expr(&inner));
                i += 1;
                continue;
            }
        }
        let tok = tokens[i].clone();
        items.push(LispOutput::Tokens(quote! { #tok }));
        i += 1;
    }
    items
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
                if i + 1 < tokens.len()
                    && is_punct(&tokens[i], ':') && i + 1 < tokens.len() && is_punct(&tokens[i + 1], ':') {
                        result.push(tokens[i].clone());
                        result.push(tokens[i + 1].clone());
                        i += 2;
                        continue;
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

// ─── LispOutput: typed output from S-expression evaluation ────────────────────
// Represents the result of evaluating an S-expression, distinguishing between
// expressions, statements, items, and raw token fallbacks.

enum LispOutput {
    Expr(syn::Expr),
    #[allow(dead_code)]
    Stmt(syn::Stmt),
    #[allow(dead_code)]
    Item(syn::Item),
    /// Fallback for forms that don't map cleanly to a single syn node
    Tokens(TokenStream2),
}

impl ToTokens for LispOutput {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        match self {
            LispOutput::Expr(e) => e.to_tokens(tokens),
            LispOutput::Stmt(s) => s.to_tokens(tokens),
            LispOutput::Item(i) => i.to_tokens(tokens),
            LispOutput::Tokens(t) => t.to_tokens(tokens),
        }
    }
}

impl LispOutput {
    /// Convert to syn::Expr, wrapping non-expression forms in Expr::Verbatim.
    fn into_expr(self) -> syn::Expr {
        match self {
            LispOutput::Expr(e) => e,
            other => syn::Expr::Verbatim(other.to_token_stream()),
        }
    }
}

// ─── Helpers for syn AST construction ─────────────────────────────────────────

/// Create a syn::Expr from raw tokens (fallback for complex forms).
fn verbatim_expr(ts: TokenStream2) -> syn::Expr {
    syn::Expr::Verbatim(ts)
}

/// Wrap an expression in parentheses to preserve precedence.
fn paren_wrap(expr: syn::Expr) -> syn::Expr {
    match &expr {
        // Don't wrap atoms that don't need parentheses
        syn::Expr::Lit(_) | syn::Expr::Path(_) | syn::Expr::Paren(_) => expr,
        _ => syn::Expr::Paren(syn::ExprParen {
            attrs: vec![],
            paren_token: syn::token::Paren::default(),
            expr: Box::new(expr),
        }),
    }
}

/// Collect body items from eval_lisp_expr results and convert to syn::Stmts.
/// Adds semicolons to non-last expression stmts for proper separation in blocks.
fn build_block_stmts(items: Vec<LispOutput>) -> Vec<syn::Stmt> {
    let len = items.len();
    items.into_iter().enumerate().map(|(i, output)| {
        let is_last = i == len - 1;
        match output {
            LispOutput::Expr(e) => {
                if is_last {
                    syn::Stmt::Expr(e, None) // Last: no semicolon (block return value)
                } else {
                    syn::Stmt::Expr(e, Some(syn::token::Semi::default()))
                }
            }
            LispOutput::Stmt(s) => s, // Already has proper semicolons
            LispOutput::Item(item) => syn::Stmt::Item(item),
            LispOutput::Tokens(t) => {
                if is_last {
                    syn::Stmt::Expr(syn::Expr::Verbatim(t), None)
                } else {
                    // Add semicolon for non-last items; if tokens already end with `;`,
                    // the double `;;` is harmless (empty statement).
                    syn::Stmt::Expr(syn::Expr::Verbatim(t), Some(syn::token::Semi::default()))
                }
            }
        }
    }).collect()
}

// ─── Operator dispatch helpers for eval_lisp_expr ────────────────────────────
// These eliminate the repetitive per-operator match arms in the Punct branch.

/// Evaluate a binary operation: `(op a b)` → `a op b`
fn eval_binary_op(operands: &[TokenTree], op: syn::BinOp) -> syn::Expr {
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
fn eval_variadic_op(operands: &[TokenTree], op: syn::BinOp) -> syn::Expr {
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
fn eval_compound_assign(rest: &[TokenTree], op_char: char) -> TokenStream2 {
    let lhs: TokenStream2 = rest[..rest.len()-1].iter().cloned().collect();
    let rhs = eval_lisp_arg(&rest[rest.len()-1..]);
    let op = Punct::new(op_char, Spacing::Joint);
    quote! { #lhs #op = #rhs; }
}

/// Dispatch punctuation-based expressions in eval_lisp_expr.
/// Returns Some(result) if handled, None otherwise.
fn eval_punct_expr(tokens: &[TokenTree]) -> Option<LispOutput> {
    let TokenTree::Punct(p) = &tokens[0] else { return None };
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
                                    return Some(LispOutput::Expr(syn::Expr::Range(syn::ExprRange {
                                        attrs: vec![],
                                        start: Some(Box::new(a)),
                                        limits: syn::RangeLimits::Closed(syn::token::DotDotEq::default()),
                                        end: Some(Box::new(b)),
                                    })));
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
                ('=', '=') if tokens.len() >= 4 => return Some(LispOutput::Expr(eval_binary_op(&tokens[2..], syn::BinOp::Eq(syn::token::EqEq::default())))),
                ('!', '=') if tokens.len() >= 4 => return Some(LispOutput::Expr(eval_binary_op(&tokens[2..], syn::BinOp::Ne(syn::token::Ne::default())))),
                ('>', '=') if tokens.len() >= 4 => return Some(LispOutput::Expr(eval_binary_op(&tokens[2..], syn::BinOp::Ge(syn::token::Ge::default())))),
                ('<', '=') if tokens.len() >= 4 => return Some(LispOutput::Expr(eval_binary_op(&tokens[2..], syn::BinOp::Le(syn::token::Le::default())))),
                // Logical operators: operands start at index 2
                ('&', '&') if tokens.len() >= 4 => return Some(LispOutput::Expr(eval_binary_op(&tokens[2..], syn::BinOp::And(syn::token::AndAnd::default())))),
                ('|', '|') if tokens.len() >= 4 => return Some(LispOutput::Expr(eval_binary_op(&tokens[2..], syn::BinOp::Or(syn::token::OrOr::default())))),
                // Compound assignment operators: (op= lhs... rhs)
                ('+', '=') | ('-', '=') | ('*', '=') | ('/', '=') | ('%', '=')
                | ('&', '=') | ('|', '=') | ('^', '=') if tokens.len() >= 4 => {
                    return Some(LispOutput::Tokens(eval_compound_assign(&tokens[2..], ch)));
                }
                // Shift operators: (<< a b) → a << b, (>> a b) → a >> b
                ('<', '<') if tokens.len() >= 4 => {
                    // Check for <<= (shift-left-assign): third token is '='
                    if tokens.len() >= 5 && is_punct(&tokens[2], '=') {
                        let rest = &tokens[3..];
                        let lhs: TokenStream2 = rest[..rest.len()-1].iter().cloned().collect();
                        let rhs = eval_lisp_arg(&rest[rest.len()-1..]);
                        return Some(LispOutput::Tokens(quote! { #lhs <<= #rhs; }));
                    }
                    return Some(LispOutput::Expr(eval_binary_op(&tokens[2..], syn::BinOp::Shl(syn::token::Shl::default()))));
                }
                ('>', '>') if tokens.len() >= 4 => {
                    // Check for >>= (shift-right-assign): third token is '='
                    if tokens.len() >= 5 && is_punct(&tokens[2], '=') {
                        let rest = &tokens[3..];
                        let lhs: TokenStream2 = rest[..rest.len()-1].iter().cloned().collect();
                        let rhs = eval_lisp_arg(&rest[rest.len()-1..]);
                        return Some(LispOutput::Tokens(quote! { #lhs >>= #rhs; }));
                    }
                    return Some(LispOutput::Expr(eval_binary_op(&tokens[2..], syn::BinOp::Shr(syn::token::Shr::default()))));
                }
                _ => {}
            }
        }
    }

    // ── Single-character operators ──
    match ch {
        // Variadic arithmetic: (+ a b ...), (- a b ...), (* a b ...), (/ a b ...), (% a b)
        '+' if tokens.len() >= 3 => return Some(LispOutput::Expr(eval_variadic_op(&tokens[1..], syn::BinOp::Add(syn::token::Plus::default())))),
        '-' if tokens.len() >= 3 => return Some(LispOutput::Expr(eval_variadic_op(&tokens[1..], syn::BinOp::Sub(syn::token::Minus::default())))),
        '*' if tokens.len() >= 3 => return Some(LispOutput::Expr(eval_variadic_op(&tokens[1..], syn::BinOp::Mul(syn::token::Star::default())))),
        '/' if tokens.len() >= 3 => return Some(LispOutput::Expr(eval_variadic_op(&tokens[1..], syn::BinOp::Div(syn::token::Slash::default())))),
        '&' if tokens.len() >= 3 => return Some(LispOutput::Expr(eval_variadic_op(&tokens[1..], syn::BinOp::BitAnd(syn::token::And::default())))),
        '|' if tokens.len() >= 3 => return Some(LispOutput::Expr(eval_variadic_op(&tokens[1..], syn::BinOp::BitOr(syn::token::Or::default())))),
        '^' if tokens.len() >= 3 => return Some(LispOutput::Expr(eval_variadic_op(&tokens[1..], syn::BinOp::BitXor(syn::token::Caret::default())))),
        '%' if tokens.len() == 3 => {
            return Some(LispOutput::Expr(eval_binary_op(&tokens[1..], syn::BinOp::Rem(syn::token::Percent::default()))));
        }
        // Simple comparison: (> a b), (< a b)
        '>' if tokens.len() == 3 => {
            return Some(LispOutput::Expr(eval_binary_op(&tokens[1..], syn::BinOp::Gt(syn::token::Gt::default()))));
        }
        '<' if tokens.len() == 3 => {
            return Some(LispOutput::Expr(eval_binary_op(&tokens[1..], syn::BinOp::Lt(syn::token::Lt::default()))));
        }
        // Simple assignment: (= var rhs...) or (= complex.lhs rhs)
        '=' if tokens.len() >= 3 => {
            // If the first token after `=` is a simple ident NOT followed by `.`, `::`, or `[`,
            // treat it as the LHS variable and everything after as the RHS expression.
            // This correctly handles (= num Some(i + 1)) → num = Some(i + 1);
            if let TokenTree::Ident(_) = &tokens[1] {
                let is_complex_lhs = tokens.len() > 2
                    && (is_punct(&tokens[2], '.') || is_punct(&tokens[2], ':')
                        || matches!(&tokens[2], TokenTree::Group(g) if g.delimiter() == Delimiter::Bracket));
                if !is_complex_lhs {
                    let lhs = &tokens[1];
                    let rhs = eval_lisp_arg(&tokens[2..]);
                    return Some(LispOutput::Tokens(quote! { #lhs = #rhs; }));
                }
            }
            // Complex LHS (e.g., self.x, v[0]): last token is RHS, everything else is LHS
            let lhs: TokenStream2 = tokens[1..tokens.len()-1].iter().cloned().collect();
            let rhs = eval_lisp_arg(&tokens[tokens.len()-1..]);
            return Some(LispOutput::Tokens(quote! { #lhs = #rhs; }));
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
        // Field access: (. obj field1 field2 ...)
        '.' if tokens.len() >= 3 => {
            let obj = eval_lisp_arg(&tokens[1..2]);
            let fields_ts: TokenStream2 = tokens[2..].iter().map(|f| {
                let f = f.clone();
                quote! { .#f }
            }).collect();
            return Some(LispOutput::Tokens(quote! { #obj #fields_ts }));
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
fn eval_lisp_expr(tokens: &[TokenTree]) -> LispOutput {
    if tokens.is_empty() {
        return LispOutput::Tokens(quote! {});
    }

    // Check for labeled forms: ('label loop/while/for/block ...)
    if tokens.len() >= 3 {
        if let TokenTree::Punct(p) = &tokens[0] {
            if p.as_char() == '\'' {
                if let TokenTree::Ident(_) = &tokens[1] {
                    let tick = &tokens[0];
                    let label = &tokens[1];
                    if tokens.len() > 2 {
                        if let TokenTree::Ident(kw) = &tokens[2] {
                            match kw.to_string().as_str() {
                                "loop" => {
                                    let body_items = parse_body_items(tokens, 3);
                                    return LispOutput::Tokens(quote! { #tick #label : loop { #(#body_items);* } });
                                }
                                "while" => {
                                    let inner_result = eval_while(&tokens[3..]);
                                    return LispOutput::Tokens(quote! { #tick #label : #inner_result });
                                }
                                "for" => {
                                    let inner_result = eval_for(&tokens[3..]);
                                    return LispOutput::Tokens(quote! { #tick #label : #inner_result });
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
                "vec" | "tuple" | "array" | "block" | "loop"
                | "break" | "continue" | "return"
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
                                    if inner.len() >= 2 && is_punct(&inner[0], '.') && is_punct(&inner[1], '.') {
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
                            let all_idents = tokens[field_start..].iter().all(|t| matches!(t, TokenTree::Ident(_)));
                            if all_idents {
                                let field_names: Vec<&TokenTree> = tokens[field_start..].iter().collect();
                                return LispOutput::Tokens(quote! { #struct_name_ts { #(#field_names),* } });
                            }
                            // Otherwise → tuple struct construction
                            let args: Vec<syn::Expr> = tokens[field_start..]
                                .iter()
                                .map(|t| eval_lisp_arg(std::slice::from_ref(t)))
                                .collect();
                            return LispOutput::Tokens(quote! { #struct_name_ts(#(#args),*) });
                        }

                        if let Some(spread_ts) = spread {
                            return LispOutput::Tokens(quote! { #struct_name_ts { #(#fields,)* #spread_ts } });
                        }
                        return LispOutput::Tokens(quote! { #struct_name_ts { #(#fields),* } });
                    }
                }
                "r#struct" | "struct" => {
                    // (struct - lit Name (field val) ...)
                    if tokens.len() >= 4
                        && is_punct(&tokens[1], '-')
                    {
                        if let TokenTree::Ident(lit_id) = &tokens[2] {
                            if *lit_id == "lit" {
                                let struct_name = &tokens[3];
                                let mut fields = Vec::new();
                                for tt in &tokens[4..] {
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
                                return LispOutput::Tokens(quote! { #struct_name { #(#fields),* } });
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
                    if tokens.len() == 2 {
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
                "rust" => {
                    // (rust { code }) or (rust stmt...)
                    if tokens.len() >= 2 {
                        if let TokenTree::Group(g) = &tokens[1] {
                            if g.delimiter() == Delimiter::Brace {
                                let inner = g.stream();
                                return LispOutput::Tokens(quote! { { #inner } });
                            }
                        }
                    }
                    // (rust stmt...) — pass through as raw Rust
                    let rest: TokenStream2 = tokens[1..].iter().cloned().collect();
                    return LispOutput::Tokens(quote! { #rest });
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
                                let fallback_ty: TokenStream2 = tokens[2..].iter().cloned().collect();
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
                        lit: syn::Lit::Bool(syn::LitBool { value: false, span: id.span() }),
                    }));
                }
                "true" => {
                    return LispOutput::Expr(syn::Expr::Lit(syn::ExprLit {
                        attrs: vec![],
                        lit: syn::Lit::Bool(syn::LitBool { value: true, span: id.span() }),
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
        if let TokenTree::Ident(_) = &tokens[0] {
            if is_punct(&tokens[1], '!') {
                // Collect the macro name (may be path::name)
                let macro_name = &tokens[0];
                let args: Vec<syn::Expr> = tokens[2..]
                    .iter()
                    .map(|t| eval_lisp_arg(std::slice::from_ref(t)))
                    .collect();
                return LispOutput::Tokens(quote! { #macro_name ! (#(#args),*) });
            }
        }
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
                    let args: Vec<syn::Expr> = tokens[path_end..]
                        .iter()
                        .map(|t| eval_lisp_arg(std::slice::from_ref(t)))
                        .collect();
                    return LispOutput::Tokens(quote! { #path_ts(#(#args),*) });
                } else {
                    // No args
                    // `self.x.y` is field access
                    if *first == "self" {
                        return LispOutput::Tokens(quote! { #path_ts });
                    }
                    // Non-self: each .ident is a zero-arg method call
                    // obj.m1.m2 → obj.m1().m2()
                    let first_tok = &tokens[0];
                    let mut result = quote! { #first_tok };
                    let mut j = 2; // skip first ident and first dot
                    while j < path_end {
                        let method = &tokens[j];
                        result = quote! { #result.#method() };
                        j += 2; // skip ident and next dot
                    }
                    return LispOutput::Tokens(result);
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
                    return LispOutput::Tokens(quote! { #path_ts() });
                }
                // Check for path::name! pattern (macro invocation)
                if !rest.is_empty() && is_punct(&rest[0], '!') {
                    let args: Vec<syn::Expr> = rest[1..]
                        .iter()
                        .map(|t| eval_lisp_arg(std::slice::from_ref(t)))
                        .collect();
                    return LispOutput::Tokens(quote! { #path_ts ! (#(#args),*) });
                }
                let args: Vec<syn::Expr> = rest
                    .iter()
                    .map(|t| eval_lisp_arg(std::slice::from_ref(t)))
                    .collect();
                return LispOutput::Tokens(quote! { #path_ts(#(#args),*) });
            }
        }
    }

    // Default: treat first ident as function call
    if let TokenTree::Ident(id) = &tokens[0] {
        let args: Vec<syn::Expr> = tokens[1..]
            .iter()
            .map(|t| eval_lisp_arg(std::slice::from_ref(t)))
            .collect();
        return LispOutput::Tokens(quote! { #id(#(#args),*) });
    }

    // Fallback: emit tokens as-is
    let ts: TokenStream2 = tokens.iter().cloned().collect();
    LispOutput::Tokens(quote! { #ts })
}

fn eval_lisp_arg(tokens: &[TokenTree]) -> syn::Expr {
    if tokens.len() == 1 {
        if let TokenTree::Group(g) = &tokens[0] {
            if g.delimiter() == Delimiter::Parenthesis {
                return eval_lisp_expr(&g.stream().into_iter().collect::<Vec<_>>()).into_expr();
            }
        }
        let t = &tokens[0];
        return verbatim_expr(quote! { #t });
    }
    // Multi-token: treat as expression
    let ts: TokenStream2 = tokens.iter().cloned().collect();
    verbatim_expr(quote! { #ts })
}

fn eval_if(tokens: &[TokenTree]) -> syn::Expr {
    if tokens.is_empty() { return verbatim_expr(quote! {}); }

    // Check for `if let` form: (if let (Pat = expr) then else)
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
    if tokens.len() >= 3 {
        let then_branch = eval_lisp_arg(&tokens[1..2]);
        let else_branch = eval_lisp_arg(&tokens[2..3]);
        verbatim_expr(quote! { if #cond { #then_branch } else { #else_branch } })
    } else if tokens.len() == 2 {
        let then_branch = eval_lisp_arg(&tokens[1..2]);
        verbatim_expr(quote! { if #cond { #then_branch } })
    } else {
        verbatim_expr(quote! { if #cond {} })
    }
}

fn eval_cond(tokens: &[TokenTree]) -> syn::Expr {
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
                    return verbatim_expr(quote! { { #(#body);* } });
                }

                // (cond body...) — regular condition branch
                let cond = eval_lisp_arg(&inner[0..1]);
                let body: Vec<syn::Expr> = inner[1..].iter().map(|t| {
                    eval_lisp_arg(std::slice::from_ref(t))
                }).collect();
                let rest = eval_cond(&tokens[1..]);

                if tokens.len() > 1 {
                    return verbatim_expr(quote! { if #cond { #(#body);* } else #rest });
                } else {
                    return verbatim_expr(quote! { if #cond { #(#body);* } });
                }
            }
        }
    }
    verbatim_expr(quote! {})
}

fn eval_let(tokens: &[TokenTree]) -> LispOutput {
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

    // (let mut var val) or (let var val)
    let mut i = 0;
    let is_mut = is_ident(&tokens[i], "mut");
    if is_mut { i += 1; }

    if i >= tokens.len() { return LispOutput::Tokens(quote! {}); }

    // Check for typed let: (let (var Type) val)
    // Only treat as typed let if the first inner element is an ident
    // and the remainder is a valid Rust type (to avoid misinterpreting patterns like (a, b)).
    if let TokenTree::Group(g) = &tokens[i] {
        if g.delimiter() == Delimiter::Parenthesis {
            let inner: Vec<TokenTree> = g.stream().into_iter().collect();
            if inner.len() >= 2 {
                if let TokenTree::Ident(_) = &inner[0] {
                    let raw_type: TokenStream2 = inner[1..].iter().cloned().collect();
                    if syn::parse2::<syn::Type>(raw_type.clone()).is_ok() {
                        let var_name = &inner[0];
                        let span = var_name.span();
                        let var_type = validate_type(raw_type);
                        i += 1;
                        if i < tokens.len() {
                            // Consume ALL remaining tokens as the value expression.
                            // This handles multi-token values like `Some(0)` which are
                            // an ident followed by a paren group. In S-expression let,
                            // the form is (let (var Type) val) with no trailing tokens.
                            let val = eval_lisp_arg(&tokens[i..]);
                            if is_mut {
                                return LispOutput::Tokens(quote_spanned! { span => let mut #var_name: #var_type = #val; });
                            } else {
                                return LispOutput::Tokens(quote_spanned! { span => let #var_name: #var_type = #val; });
                            }
                        }
                    }
                }
            }
        }
    }

    let var_name = &tokens[i];
    let span = var_name.span();
    i += 1;
    if i < tokens.len() {
        // Consume ALL remaining tokens as the value. Handles multi-token values
        // like `Some(0)` (ident + paren group) and `Vec::new()` (path + parens).
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

fn eval_for(tokens: &[TokenTree]) -> syn::Expr {
    // (for var in iter (body)...) or (for (pat) in iter (body)...)
    if tokens.len() < 3 { return verbatim_expr(quote! {}); }

    // Check if first token is a pattern group
    let (var_ts, in_idx) = if let TokenTree::Group(g) = &tokens[0] {
        if g.delimiter() == Delimiter::Parenthesis {
            let inner = g.stream();
            (quote! { (#inner) }, 1)
        } else {
            let v = &tokens[0];
            (quote! { #v }, 1)
        }
    } else {
        let v = &tokens[0];
        (quote! { #v }, 1)
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
    verbatim_expr(quote! { for #var_ts in #iter_expr { #(#body);* } })
}

fn eval_while(tokens: &[TokenTree]) -> syn::Expr {
    if tokens.is_empty() { return verbatim_expr(quote! {}); }

    // Check for while let: (while let (Pat = cond) body...)
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
    verbatim_expr(quote! { while #cond { #(#body);* } })
}

fn find_fat_arrow(tokens: &[TokenTree]) -> Option<usize> {
    (0..tokens.len().saturating_sub(1))
        .find(|&i| is_punct(&tokens[i], '=') && is_punct(&tokens[i + 1], '>'))
}

fn eval_match(tokens: &[TokenTree]) -> syn::Expr {
    if tokens.is_empty() { return verbatim_expr(quote! {}); }
    let expr = eval_lisp_arg(&tokens[0..1]);
    let mut arms = Vec::new();

    for tt in &tokens[1..] {
        if let TokenTree::Group(g) = tt {
            if g.delimiter() == Delimiter::Parenthesis {
                let inner: Vec<TokenTree> = g.stream().into_iter().collect();
                if let Some(arrow_pos) = find_fat_arrow(&inner) {
                    let pat_tokens = &inner[..arrow_pos];
                    let body = &inner[arrow_pos + 2..]; // skip = and >

                    // Split pattern and optional guard: `pat if guard_expr`
                    let (pat_ts, guard_ts) = split_pattern_guard(pat_tokens);

                    // Match body evaluation: mirrors old lisp_match_arg! behavior.
                    // A single paren-group body gets its contents unwrapped and evaluated
                    // as match-arm values (single idents are values, not function calls).
                    let body_ts = if body.len() == 1 {
                        if let TokenTree::Group(bg) = &body[0] {
                            if bg.delimiter() == Delimiter::Parenthesis {
                                let body_inner: Vec<TokenTree> = bg.stream().into_iter().collect();
                                eval_match_body(&body_inner)
                            } else {
                                eval_lisp_arg(body).to_token_stream()
                            }
                        } else {
                            let t = &body[0];
                            quote! { #t }
                        }
                    } else if body.is_empty() {
                        quote! { {} }
                    } else {
                        let items: Vec<syn::Expr> = body.iter()
                            .map(|t| eval_lisp_arg(std::slice::from_ref(t)))
                            .collect();
                        quote! { { #(#items);* } }
                    };
                    arms.push(quote! { #pat_ts #guard_ts => #body_ts });
                }
            }
        }
    }

    verbatim_expr(quote! { match #expr { #(#arms),* } })
}

/// Evaluate match arm body contents. Single idents are treated as values (not function calls),
/// matching the old `lisp_match_arg!` behavior.
fn eval_match_body(tokens: &[TokenTree]) -> TokenStream2 {
    if tokens.len() == 1 {
        let t = &tokens[0];
        if let TokenTree::Group(g) = t {
            if g.delimiter() == Delimiter::Parenthesis {
                // (expr) in body → evaluate as lisp expression
                return eval_lisp_expr(&g.stream().into_iter().collect::<Vec<_>>()).to_token_stream();
            }
        }
        // Single non-group token → return as value
        return quote! { #t };
    }
    // Multi-token → evaluate as expression
    eval_lisp_expr(tokens).to_token_stream()
}

/// Split match arm pattern into pattern and optional guard.
/// `n if (> n 0)` → pattern `n`, guard `if n > 0`
fn split_pattern_guard(tokens: &[TokenTree]) -> (TokenStream2, TokenStream2) {
    for (i, t) in tokens.iter().enumerate() {
        if is_ident(t, "if") {
            let pat: TokenStream2 = tokens[..i].iter().cloned().collect();
            let guard = eval_lisp_arg(&tokens[i + 1..]);
            return (pat, quote! { if #guard });
        }
    }
    let pat: TokenStream2 = tokens.iter().cloned().collect();
    (pat, quote! {})
}

// ─── Item-level forms: const, static, type ───────────────────────────────────
// Handles: [vis] const NAME TYPE [=] VALUE
//          [vis] static [mut] NAME TYPE [=] VALUE
//          [vis] type NAME = TARGET
// Produces complete items including trailing semicolons.
fn eval_item_form(tokens: &[TokenTree]) -> Option<LispOutput> {
    if tokens.is_empty() { return None; }

    // Parse optional visibility prefix
    let (vis_ts, vis_consumed) = parse_visibility(tokens);
    let rest = &tokens[vis_consumed..];
    if rest.is_empty() { return None; }

    // Determine the keyword: const, static, or type
    if is_ident(&rest[0], "const") {
        // Skip "const fn" — that's a function, not a const variable
        if rest.len() >= 2 && is_ident(&rest[1], "fn") {
            return None;
        }
        return Some(eval_const_static(rest, &vis_ts, false, false));
    }

    if is_ident(&rest[0], "static") {
        let is_mut = rest.len() >= 2 && is_ident(&rest[1], "mut");
        return Some(eval_const_static(rest, &vis_ts, true, is_mut));
    }

    if is_ident(&rest[0], "type") {
        return Some(eval_type_alias(rest, &vis_ts));
    }

    None
}

/// Parse: const/static [mut] NAME TYPE [=] VALUE
/// `keyword_rest` starts at "const" or "static".
fn eval_const_static(keyword_rest: &[TokenTree], vis: &TokenStream2, is_static: bool, is_mut: bool) -> LispOutput {
    let mut i = 1; // skip "const" or "static"
    if is_mut { i += 1; } // skip "mut"

    if i >= keyword_rest.len() {
        abort!(keyword_rest[0].span(), "expected name after const/static keyword");
    }

    let name = &keyword_rest[i];
    i += 1;

    if i >= keyword_rest.len() {
        abort!(name.span(), "expected type after name in const/static");
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
            abort!(keyword_rest[0].span(), "expected value in const/static");
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

    LispOutput::Tokens(quote! { #vis #item_kw #name : #type_ts = #value_ts; })
}

/// Parse: type NAME = TARGET
/// `keyword_rest` starts at "type".
fn eval_type_alias(keyword_rest: &[TokenTree], vis: &TokenStream2) -> LispOutput {
    // type NAME = TARGET
    if keyword_rest.len() < 4 {
        abort!(keyword_rest[0].span(), "expected: type NAME = TARGET");
    }

    let name = &keyword_rest[1]; // NAME
    // keyword_rest[2] should be `=`
    if !is_punct(&keyword_rest[2], '=') {
        abort!(keyword_rest[2].span(), "expected `=` in type alias");
    }
    let target: TokenStream2 = keyword_rest[3..].iter().cloned().collect();
    let target_validated = validate_type(target);

    LispOutput::Tokens(quote! { #vis type #name = #target_validated; })
}

fn eval_closure(tokens: &[TokenTree]) -> syn::Expr {
    if tokens.is_empty() { return verbatim_expr(quote! { || {} }); }

    let mut i = 0;
    let is_move = i < tokens.len() && is_ident(&tokens[i], "move");
    if is_move { i += 1; }

    // Parse parameter list (a paren group containing (name Type) pairs)
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
                                let name = &inner[0];
                                let raw_type: TokenStream2 = inner[1..].iter().cloned().collect();
                                let typ = validate_type(raw_type);
                                params.push(quote! { #name: #typ });
                            } else if inner.len() == 1 {
                                let name = &inner[0];
                                params.push(quote! { #name });
                            }
                        }
                    }
                }
                i += 1;
            }
        }
    }

    // Check for return type: -> Type
    let mut return_type = None;
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
                            return_type = Some(validate_type(rt));
                        }
                    }
                }
            }
        }
    }

    // Parse body items
    let body_items = parse_body_items(tokens, i);

    let move_kw = if is_move { quote! { move } } else { quote! {} };
    let ret = match &return_type {
        Some(r) => quote! { -> #r },
        None => quote! {},
    };

    verbatim_expr(quote! { #move_kw |#(#params),*| #ret { #(#body_items);* } })
}

// ─── Shared fn signature parsing ─────────────────────────────────────────────
// Extracted from parse_fn and parse_impl_body_item to eliminate duplication.
// Parses: attributes, qualifiers, fn keyword, name, generics, params, return type, where clause.

struct FnSignature {
    attrs: TokenStream2,
    quals: TokenStream2,
    name: proc_macro2::Ident,
    generics: Option<TokenStream2>,
    params: Vec<TokenStream2>,
    return_type: Option<TokenStream2>,
    where_clause: Option<TokenStream2>,
    body_start: usize,
}

/// Parse the fn signature portion from tokens, returning None if `fn` keyword is not found.
fn parse_fn_signature(tokens: &[TokenTree]) -> syn::Result<Option<(FnSignature, /* not-a-fn fallback tokens */ Option<TokenStream2>)>> {
    let mut i = 0;

    // 1. Collect attributes
    let attrs_ts = parse_attributes(tokens, &mut i);

    // 2. Collect qualifiers: pub, pub(crate), pub(super), const, unsafe, async, extern "C"
    let mut qualifiers = Vec::new();
    loop {
        if i >= tokens.len() { break; }
        match &tokens[i] {
            TokenTree::Ident(id) if *id == "pub" => {
                qualifiers.push(tokens[i].clone());
                i += 1;
                if i < tokens.len() {
                    if let TokenTree::Group(g) = &tokens[i] {
                        if g.delimiter() == Delimiter::Parenthesis {
                            qualifiers.push(tokens[i].clone());
                            i += 1;
                        }
                    }
                }
            }
            TokenTree::Ident(id) if *id == "const" || *id == "unsafe" || *id == "async" => {
                qualifiers.push(tokens[i].clone());
                i += 1;
            }
            TokenTree::Ident(id) if *id == "extern" => {
                qualifiers.push(tokens[i].clone());
                i += 1;
                if i < tokens.len() && is_literal_like(&tokens[i]) {
                    qualifiers.push(tokens[i].clone());
                    i += 1;
                }
            }
            _ => break,
        }
    }
    let quals_ts: TokenStream2 = qualifiers.into_iter().collect();

    // 3. Skip `fn` keyword — if absent, this isn't a function
    if i < tokens.len() && is_ident(&tokens[i], "fn") {
        i += 1;
    } else {
        return Ok(None);
    }

    // 4. Function name
    let fn_name = if i < tokens.len() {
        if let TokenTree::Ident(id) = &tokens[i] {
            let id = id.clone();
            i += 1;
            id
        } else {
            return Err(syn::Error::new(tokens[i].span(), "expected function name"));
        }
    } else {
        let span = if i > 0 { tokens[i - 1].span() } else { Span::call_site() };
        return Err(syn::Error::new(span, "expected function name after `fn`"));
    };

    // 5. Check for <...> generic params
    let generics = parse_generics_params(tokens, &mut i);

    // 6. Parse parameter list: a parenthesized group containing (name Type) pairs
    let mut params = Vec::new();
    if i < tokens.len() {
        if let TokenTree::Group(g) = &tokens[i] {
            if g.delimiter() == Delimiter::Parenthesis {
                let param_tokens: Vec<TokenTree> = g.stream().into_iter().collect();
                let mut is_first_param = true;
                for tt in &param_tokens {
                    if let TokenTree::Group(pg) = tt {
                        if pg.delimiter() == Delimiter::Parenthesis {
                            let inner: Vec<TokenTree> = pg.stream().into_iter().collect();
                            if is_first_param {
                                if let Some(self_param) = parse_self_param(&inner) {
                                    params.push(self_param);
                                    is_first_param = false;
                                    continue;
                                }
                            }
                            if inner.len() >= 2 {
                                let param_name = &inner[0];
                                let raw_type: TokenStream2 = inner[1..].iter().cloned().collect();
                                let param_type = validate_type(raw_type);
                                params.push(quote! { #param_name: #param_type });
                            }
                            is_first_param = false;
                        }
                    }
                }
                i += 1;
            }
        }
    }

    // 7. Parse return type
    let mut return_type_tokens = Vec::new();
    // Check for () as unit return type when followed by more tokens
    if i < tokens.len() {
        if let TokenTree::Group(g) = &tokens[i] {
            if g.delimiter() == Delimiter::Parenthesis && g.stream().is_empty() && i + 1 < tokens.len() {
                return_type_tokens.push(tokens[i].clone());
                i += 1;
            }
        }
    }
    if return_type_tokens.is_empty() {
        while i < tokens.len() {
            if is_ident(&tokens[i], "where") { break; }
            if let TokenTree::Group(g) = &tokens[i] {
                if g.delimiter() == Delimiter::Parenthesis {
                    break;
                }
            }
            if is_literal_like(&tokens[i]) {
                break;
            }
            return_type_tokens.push(tokens[i].clone());
            i += 1;
        }
    }
    let return_type = if return_type_tokens.is_empty() {
        None
    } else {
        let rt: TokenStream2 = return_type_tokens.into_iter().collect();
        Some(validate_type(rt))
    };

    // 8. Check for `where` clause
    let where_clause = parse_where_clause(tokens, &mut i);

    Ok(Some((FnSignature {
        attrs: attrs_ts,
        quals: quals_ts,
        name: fn_name,
        generics,
        params,
        return_type,
        where_clause,
        body_start: i,
    }, None)))
}

/// Parse a lisp-style method/fn definition from tokens inside an impl/trait body.
/// Input: the contents of a (...) group, e.g. `fn foo ((self &Self) (x i32)) RetType (body)`
/// Also handles: `type Name = Type`
/// Returns the Rust code as TokenStream2.
fn parse_impl_body_item(tokens: &[TokenTree]) -> syn::Result<LispOutput> {
    if tokens.is_empty() {
        return Ok(LispOutput::Tokens(quote! {}));
    }

    // Check for `type Name = Type`
    if is_ident(&tokens[0], "type") {
        let rest: TokenStream2 = tokens[1..].iter().cloned().collect();
        return Ok(LispOutput::Tokens(quote! { type #rest; }));
    }

    // Use shared fn signature parser
    let sig = match parse_fn_signature(tokens)? {
        Some((sig, _)) => sig,
        None => {
            // Not a fn definition — evaluate directly
            return Ok(eval_lisp_expr(tokens));
        }
    };

    // Parse body: remaining tokens are parenthesized groups or bare expressions
    let body_items = parse_body_items(tokens, sig.body_start);
    let has_body = sig.body_start < tokens.len();

    let gen = emit_generics(&sig.generics);
    let ret = emit_return_type(&sig.return_type);
    let where_cl = emit_where_clause(&sig.where_clause);
    let attrs_ts = &sig.attrs;
    let quals_ts = &sig.quals;
    let fn_name = &sig.name;
    let params = &sig.params;

    if has_body {
        Ok(LispOutput::Tokens(quote! {
            #attrs_ts
            #quals_ts fn #fn_name #gen (#(#params),*) #ret #where_cl {
                #(#body_items);*
            }
        }))
    } else {
        Ok(LispOutput::Tokens(quote! {
            #attrs_ts
            #quals_ts fn #fn_name #gen (#(#params),*) #ret #where_cl;
        }))
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
#[proc_macro_error]
pub fn lisp_assign(input: TokenStream) -> TokenStream {
    let tokens: Vec<TokenTree> = flatten_none_delim(TokenStream2::from(input).into_iter().collect());
    if tokens.len() < 3 {
        abort!(tokens.first().map_or(Span::call_site(), |t| t.span()),
               "lisp_assign! requires at least an operator, LHS, and RHS");
    }

    // Parse the operator (first 1 or 2 punctuation tokens)
    let (op_tokens, rest) = parse_assign_op(&tokens);
    if op_tokens.is_empty() || rest.is_empty() {
        abort!(tokens[0].span(), "lisp_assign! invalid operator or missing operands");
    }

    let op_ts: TokenStream2 = op_tokens.into_iter().collect();

    // Split rest into LHS (all but last) and RHS (last token)
    if rest.len() < 2 {
        abort!(rest[0].span(), "lisp_assign! requires both LHS and RHS");
    }

    let lhs_tokens = &rest[..rest.len() - 1];
    let rhs_token = &rest[rest.len() - 1];

    let lhs_ts: TokenStream2 = lhs_tokens.iter().cloned().collect();

    // If RHS is a parenthesized group, recurse through lisp!
    let rhs_ts = match rhs_token {
        TokenTree::Group(g) if g.delimiter() == Delimiter::Parenthesis => {
            let inner: Vec<TokenTree> = g.stream().into_iter().collect();
            eval_lisp_expr(&inner).to_token_stream()
        }
        other => {
            let other = other.clone();
            quote! { #other }
        }
    };

    let span = lhs_tokens.first().map_or(Span::call_site(), |t| t.span());
    let result = quote_spanned! { span =>
        #lhs_ts #op_ts #rhs_ts;
    };
    debug_expansion("lisp_assign!", &result);
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
                    (op, &tokens[i + 1..])
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
                    (vec![], tokens)
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
                    (vec![], tokens)
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
                    (vec![], tokens)
                }
                _ => (vec![], tokens),
            }
        }
        _ => (vec![], tokens),
    }
}

// ─── lisp_impl! ─────────────────────────────────────────────────────────────
//
// Usage: lisp_impl!(GenericParams Type (method)...)
//        lisp_impl!(GenericParams Trait for Type (method)...)
//        lisp_impl!(Type (method)...)
#[proc_macro]
#[proc_macro_error]
pub fn lisp_impl(input: TokenStream) -> TokenStream {
    let tokens: Vec<TokenTree> = flatten_none_delim(TokenStream2::from(input).into_iter().collect());
    let result = parse_impl(&tokens);
    match result {
        Ok(item) => {
            let ts = item.to_token_stream();
            debug_expansion("lisp_impl!", &ts);
            ts.into()
        }
        Err(e) => e.to_compile_error().into(),
    }
}

fn parse_impl(tokens: &[TokenTree]) -> syn::Result<syn::Item> {
    let mut i = 0;

    // 1. Check for <...> generic params
    let generics = parse_generics_params(tokens, &mut i);

    // 2. Consume the first type/trait path
    let (first_path, rest) = consume_type_path(&tokens[i..]);
    if first_path.is_empty() {
        let span = tokens.get(i).map_or(Span::call_site(), |t| t.span());
        return Err(syn::Error::new(span, "lisp_impl!: expected type or trait name"));
    }
    let first_path_ts: TokenStream2 = first_path.into_iter().collect();
    i = tokens.len() - rest.len();

    // 3. Check for `for` keyword
    let (trait_path, impl_type) = if i < tokens.len() && is_ident(&tokens[i], "for") {
        i += 1; // skip `for`
        let (type_path, rest2) = consume_type_path(&tokens[i..]);
        if type_path.is_empty() {
            let span = tokens.get(i).map_or(tokens[i - 1].span(), |t| t.span());
            return Err(syn::Error::new(span, "lisp_impl!: expected type after 'for'"));
        }
        let type_path_ts: TokenStream2 = type_path.into_iter().collect();
        i = tokens.len() - rest2.len();
        (Some(first_path_ts), type_path_ts)
    } else {
        (None, first_path_ts)
    };

    // 4. Check for `where` clause
    let where_clause = parse_where_clause(tokens, &mut i);

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
    let gen = emit_generics(&generics);
    let where_cl = emit_where_clause(&where_clause);
    let trait_for = match &trait_path {
        Some(t) => quote! { #t for },
        None => quote! {},
    };

    Ok(syn::Item::Verbatim(quote! {
        impl #gen #trait_for #impl_type #where_cl {
            #(#body_items)*
        }
    }))
}

// ─── lisp_trait! ────────────────────────────────────────────────────────────
#[proc_macro]
#[proc_macro_error]
pub fn lisp_trait(input: TokenStream) -> TokenStream {
    let tokens: Vec<TokenTree> = flatten_none_delim(TokenStream2::from(input).into_iter().collect());
    let result = parse_trait(&tokens);
    match result {
        Ok(item) => {
            let ts = item.to_token_stream();
            debug_expansion("lisp_trait!", &ts);
            ts.into()
        }
        Err(e) => e.to_compile_error().into(),
    }
}

fn parse_trait(tokens: &[TokenTree]) -> syn::Result<syn::Item> {
    let mut i = 0;

    // 1. Check for visibility (pub, pub(crate), etc.)
    let (vis_ts, vis_consumed) = parse_visibility(&tokens[i..]);
    i += vis_consumed;

    // 2. Consume trait name
    let trait_name = if i < tokens.len() {
        if let TokenTree::Ident(id) = &tokens[i] {
            let id = id.clone();
            i += 1;
            id
        } else {
            return Err(syn::Error::new(tokens[i].span(), "lisp_trait!: expected trait name"));
        }
    } else {
        let span = if i > 0 { tokens[i - 1].span() } else { Span::call_site() };
        return Err(syn::Error::new(span, "lisp_trait!: expected trait name"));
    };

    // 3. Check for <...> generic params
    let generics = parse_generics_params(tokens, &mut i);

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
    let where_clause = parse_where_clause(tokens, &mut i);

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

    let gen = emit_generics(&generics);
    let super_cl = match &supertraits {
        Some(s) => quote! { : #s },
        None => quote! {},
    };
    let where_cl = emit_where_clause(&where_clause);

    Ok(syn::Item::Verbatim(quote! {
        #vis_ts trait #trait_name #gen #super_cl #where_cl {
            #(#body_items)*
        }
    }))
}

// ─── lisp_enum! ─────────────────────────────────────────────────────────────
#[proc_macro]
#[proc_macro_error]
pub fn lisp_enum(input: TokenStream) -> TokenStream {
    let tokens: Vec<TokenTree> = flatten_none_delim(TokenStream2::from(input).into_iter().collect());
    let result = parse_enum(&tokens);
    match result {
        Ok(item) => {
            let ts = item.to_token_stream();
            debug_expansion("lisp_enum!", &ts);
            ts.into()
        }
        Err(e) => e.to_compile_error().into(),
    }
}

fn parse_enum(tokens: &[TokenTree]) -> syn::Result<syn::Item> {
    let mut i = 0;

    // 1. Check for attributes and pub
    let attrs_ts = parse_attributes(tokens, &mut i);

    let (vis_ts, vis_consumed) = parse_visibility(&tokens[i..]);
    i += vis_consumed;

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
            return Err(syn::Error::new(tokens[i].span(), "lisp_enum!: expected enum name"));
        }
    } else {
        let span = if i > 0 { tokens[i - 1].span() } else { Span::call_site() };
        return Err(syn::Error::new(span, "lisp_enum!: expected enum name"));
    };

    // 3. Check for <...> generic params
    let generics = parse_generics_params(tokens, &mut i);

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

    let gen = emit_generics(&generics);

    Ok(syn::Item::Verbatim(quote! {
        #attrs_ts
        #vis_ts enum #enum_name #gen {
            #(#variants),*
        }
    }))
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
        return Err(syn::Error::new(tokens[0].span(), "expected variant name"));
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
                        let raw_type: TokenStream2 = inner[1..].iter().cloned().collect();
                        let field_type = validate_type(raw_type);
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
#[proc_macro_error]
pub fn lisp_struct(input: TokenStream) -> TokenStream {
    let tokens: Vec<TokenTree> = flatten_none_delim(TokenStream2::from(input).into_iter().collect());
    let result = parse_struct(&tokens);
    match result {
        Ok(item) => {
            let ts = item.to_token_stream();
            debug_expansion("lisp_struct!", &ts);
            ts.into()
        }
        Err(e) => e.to_compile_error().into(),
    }
}

fn parse_struct(tokens: &[TokenTree]) -> syn::Result<syn::Item> {
    let mut i = 0;

    // 1. Collect attributes
    let attrs_ts = parse_attributes(tokens, &mut i);

    // 2. Check for visibility
    let (vis_ts, vis_consumed) = parse_visibility(&tokens[i..]);
    i += vis_consumed;

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
            return Err(syn::Error::new(tokens[i].span(), "lisp_struct!: expected struct name"));
        }
    } else {
        let span = if i > 0 { tokens[i - 1].span() } else { Span::call_site() };
        return Err(syn::Error::new(span, "lisp_struct!: expected struct name"));
    };

    // 4. Consume <...> generic params
    let generics = parse_generics_params(tokens, &mut i);

    // 5. Check for where clause
    let where_clause = parse_where_clause(tokens, &mut i);

    let gen = emit_generics(&generics);
    let where_cl = emit_where_clause(&where_clause);

    // 6. Parse fields — collect all remaining parenthesized groups as field groups.
    // Supports: named fields ((f T) ...), pub fields (pub (f T) ...), mixed (pub ...)(...),
    //           tuple structs (T1 T2 ...), and unit structs.
    let mut field_groups = Vec::new();
    while i < tokens.len() {
        if let TokenTree::Group(g) = &tokens[i] {
            if g.delimiter() == Delimiter::Parenthesis {
                field_groups.push(g.clone());
            }
        }
        i += 1;
    }

    if field_groups.is_empty() {
        // Unit struct
        return Ok(syn::Item::Verbatim(quote! { #attrs_ts #vis_ts struct #struct_name #gen #where_cl; }));
    }

    // Determine if named fields or tuple struct by inspecting first group contents
    let first_inner: Vec<TokenTree> = field_groups[0].stream().into_iter().collect();
    if first_inner.is_empty() {
        return Ok(syn::Item::Verbatim(quote! { #attrs_ts #vis_ts struct #struct_name #gen #where_cl; }));
    }

    // Check if this is a named-field struct: first meaningful element is a Group (field definition)
    let is_named = {
        let check_idx = if is_ident(&first_inner[0], "pub") { 1 } else { 0 };
        check_idx < first_inner.len() && matches!(&first_inner[check_idx], TokenTree::Group(_))
    };

    if is_named {
        // Named struct — process all field groups (supports mixed pub/private via multiple groups)
        let mut all_fields = Vec::new();
        for group in &field_groups {
            let inner: Vec<TokenTree> = group.stream().into_iter().collect();
            let mut field_start = 0;
            let mut is_pub = false;
            if !inner.is_empty() && is_ident(&inner[0], "pub") {
                is_pub = true;
                field_start = 1;
            }
            for tt in &inner[field_start..] {
                if let TokenTree::Group(fg) = tt {
                    if fg.delimiter() == Delimiter::Parenthesis {
                        let field_tokens: Vec<TokenTree> = fg.stream().into_iter().collect();
                        if field_tokens.len() >= 2 {
                            let field_name = &field_tokens[0];
                            let raw_type: TokenStream2 = field_tokens[1..].iter().cloned().collect();
                            let field_type = validate_type(raw_type);
                            if is_pub {
                                all_fields.push(quote! { pub #field_name: #field_type });
                            } else {
                                all_fields.push(quote! { #field_name: #field_type });
                            }
                        }
                    }
                }
            }
        }
        Ok(syn::Item::Verbatim(quote! {
            #attrs_ts
            #vis_ts struct #struct_name #gen #where_cl {
                #(#all_fields),*
            }
        }))
    } else {
        // Tuple struct — first group contains space-separated types
        let types_stream: TokenStream2 = first_inner.into_iter().collect();
        let types = parse_type_list(types_stream)
            .map_err(|e| syn::Error::new(e.span(), format!("lisp_struct! tuple fields: {}", e)))?;
        Ok(syn::Item::Verbatim(quote! {
            #attrs_ts
            #vis_ts struct #struct_name #gen #where_cl ( #(#types),* );
        }))
    }
}

// ─── lisp_fn! ───────────────────────────────────────────────────────────────
#[proc_macro]
#[proc_macro_error]
pub fn lisp_fn(input: TokenStream) -> TokenStream {
    let tokens: Vec<TokenTree> = flatten_none_delim(TokenStream2::from(input).into_iter().collect());
    let result = parse_fn(&tokens);
    match result {
        Ok(item) => {
            let ts = item.to_token_stream();
            debug_expansion("lisp_fn!", &ts);
            ts.into()
        }
        Err(e) => e.to_compile_error().into(),
    }
}

fn parse_fn(tokens: &[TokenTree]) -> syn::Result<syn::Item> {
    // Use shared fn signature parser
    let sig = match parse_fn_signature(tokens)? {
        Some((sig, _)) => sig,
        None => {
            let span = tokens.first().map_or(Span::call_site(), |t| t.span());
            return Err(syn::Error::new(span, "lisp_fn!: expected fn keyword"));
        }
    };

    // Body: remaining parenthesized groups evaluated via eval_lisp_expr
    // (consistent with parse_impl_body_item, eliminating the ::lisp::lisp! callback chain)
    let body_items = parse_body_items(tokens, sig.body_start);

    let gen = emit_generics(&sig.generics);
    let ret = emit_return_type(&sig.return_type);
    let where_cl = emit_where_clause(&sig.where_clause);
    let attrs_ts = &sig.attrs;
    let quals_ts = &sig.quals;
    let fn_name = &sig.name;
    let params = &sig.params;

    Ok(syn::Item::Verbatim(quote! {
        #attrs_ts
        #quals_ts fn #fn_name #gen (#(#params),*) #ret #where_cl {
            #(#body_items);*
        }
    }))
}

// ─── lisp_let! ──────────────────────────────────────────────────────────────
//
// Usage: lisp_let!(pattern_tokens... value_token)
//
// Handles pattern destructuring in let bindings that macro_rules! can't match.
// The last token tree is the value (possibly a parenthesized group for lisp recursion).
// Everything before the last token is the pattern (emitted verbatim).
// Validates patterns with syn::Pat when possible.
#[proc_macro]
#[proc_macro_error]
pub fn lisp_let(input: TokenStream) -> TokenStream {
    let tokens: Vec<TokenTree> = flatten_none_delim(TokenStream2::from(input).into_iter().collect());
    if tokens.len() < 2 {
        abort!(tokens.first().map_or(Span::call_site(), |t| t.span()),
               "lisp_let! requires a pattern and a value");
    }

    // Check for `mut` keyword at the start
    let mut i = 0;
    let is_mut = is_ident(&tokens[i], "mut");
    if is_mut { i += 1; }

    if tokens.len() - i < 2 {
        abort!(tokens[i].span(), "lisp_let! requires a pattern and a value");
    }

    // Pattern is everything from i to len-1, value is last token
    let pattern_tokens = &tokens[i..tokens.len() - 1];
    let value_token = &tokens[tokens.len() - 1];

    let raw_pattern: TokenStream2 = pattern_tokens.iter().cloned().collect();
    let pattern_ts = validate_pattern(raw_pattern);

    // If value is a parenthesized group, recurse through eval_lisp_expr
    let val_ts = match value_token {
        TokenTree::Group(g) if g.delimiter() == Delimiter::Parenthesis => {
            let inner: Vec<TokenTree> = g.stream().into_iter().collect();
            eval_lisp_expr(&inner).to_token_stream()
        }
        other => {
            let other = other.clone();
            quote! { #other }
        }
    };

    let mut_kw = if is_mut { quote! { mut } } else { quote! {} };

    let span = pattern_tokens.first().map_or(Span::call_site(), |t| t.span());
    let result = quote_spanned! { span =>
        let #mut_kw #pattern_ts = #val_ts;
    };
    debug_expansion("lisp_let!", &result);
    result.into()
}

// ─── lisp_eval! ─────────────────────────────────────────────────────────────
//
// The single entry point for all S-expression → Rust expression evaluation.
// This proc macro delegates to eval_lisp_expr, which is the source of truth
// for transforming lisp-style S-expressions into Rust code.
#[proc_macro]
#[proc_macro_error]
pub fn lisp_eval(input: TokenStream) -> TokenStream {
    let tokens: Vec<TokenTree> = flatten_none_delim(TokenStream2::from(input).into_iter().collect());
    let result = eval_lisp_expr(&tokens);
    let result_ts = result.to_token_stream();
    debug_expansion("lisp_eval!", &result_ts);
    result_ts.into()
}
