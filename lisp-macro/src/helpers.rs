use proc_macro2::{Delimiter, Punct, Spacing, TokenStream as TokenStream2, TokenTree};
use quote::quote;

// ─── Helper: flatten invisible delimiter groups ─────────────────────────────
// macro_rules! wraps captured fragments ($vis:vis, $expr:expr, etc.) in
// Group { delimiter: None } ("invisible groups"). When these are forwarded
// to a proc macro, the proc macro sees the wrapper instead of the raw tokens.
// This helper flattens top-level invisible groups so parsing sees raw tokens.
pub(crate) fn flatten_none_delim(tokens: Vec<TokenTree>) -> Vec<TokenTree> {
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
pub(crate) fn parse_type_list(stream: TokenStream2) -> syn::Result<Vec<syn::Type>> {
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
pub(crate) fn is_punct(tt: &TokenTree, ch: char) -> bool {
    matches!(tt, TokenTree::Punct(p) if p.as_char() == ch)
}

// ─── Helper: check if a token tree is a specific ident ───
pub(crate) fn is_ident(tt: &TokenTree, name: &str) -> bool {
    matches!(tt, TokenTree::Ident(i) if *i == name)
}

// ─── Helper: check if a token tree is a literal (or a literal wrapped in an invisible group) ───
// macro_rules! fragment captures like `$abi:literal` wrap the literal in a Group { delimiter: None }.
pub(crate) fn is_literal_like(tt: &TokenTree) -> bool {
    match tt {
        TokenTree::Literal(_) => true,
        TokenTree::Group(g) if g.delimiter() == Delimiter::None => g
            .stream()
            .into_iter()
            .any(|t| matches!(t, TokenTree::Literal(_))),
        _ => false,
    }
}

// ─── Helper: validate a token stream as a Rust type using syn::Type ───
// Returns the validated type tokens, falling back to raw tokens if syn can't parse them.
pub(crate) fn validate_type(tokens: TokenStream2) -> TokenStream2 {
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
pub(crate) fn validate_pattern(tokens: TokenStream2) -> TokenStream2 {
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
pub(crate) fn validate_generics(tokens: &TokenStream2) -> TokenStream2 {
    let wrapped = quote! { <#tokens> };
    match syn::parse2::<syn::Generics>(wrapped) {
        Ok(_) => tokens.clone(),
        Err(_) => tokens.clone(),
    }
}

// ─── Helper: consume balanced angle brackets from tokens, returning (angle_contents, rest) ───
// Expects tokens[0] to be '<'
pub(crate) fn consume_angle_brackets(tokens: &[TokenTree]) -> (Vec<TokenTree>, &[TokenTree]) {
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
pub(crate) fn consume_type_path(tokens: &[TokenTree]) -> (Vec<TokenTree>, &[TokenTree]) {
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
                    && is_punct(&tokens[i], ':')
                    && i + 1 < tokens.len()
                    && is_punct(&tokens[i + 1], ':')
                {
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
