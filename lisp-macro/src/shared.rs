use proc_macro2::{Delimiter, TokenTree, TokenStream as TokenStream2};
use proc_macro_error2::abort;
use quote::quote;

use crate::helpers::{is_punct, is_ident, validate_generics, consume_angle_brackets};
use crate::output::LispOutput;
use crate::expr::eval_lisp_expr;

// ─── Helper: parse a self parameter from tokens inside a parameter group ───
// Returns Some(token_stream) if it's a self parameter form, None otherwise.
// Only handles shorthand forms where self doesn't have a type annotation;
// (self Type) falls through to regular parameter parsing to preserve token spans.
pub(crate) fn parse_self_param(inner: &[TokenTree]) -> Option<TokenStream2> {
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
pub(crate) fn parse_visibility(tokens: &[TokenTree]) -> (TokenStream2, usize) {
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
pub(crate) fn parse_attributes(tokens: &[TokenTree], i: &mut usize) -> TokenStream2 {
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
pub(crate) fn parse_generics_params(tokens: &[TokenTree], i: &mut usize) -> Option<TokenStream2> {
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
pub(crate) fn parse_where_clause(tokens: &[TokenTree], i: &mut usize) -> Option<TokenStream2> {
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
pub(crate) fn emit_generics(gen: &Option<TokenStream2>) -> TokenStream2 {
    match gen {
        Some(g) => quote! { <#g> },
        None => quote! {},
    }
}

/// Emit `-> R` from an optional return type token stream.
pub(crate) fn emit_return_type(ret: &Option<TokenStream2>) -> TokenStream2 {
    match ret {
        Some(r) => quote! { -> #r },
        None => quote! {},
    }
}

/// Emit `where W` from an optional where clause token stream.
pub(crate) fn emit_where_clause(wc: &Option<TokenStream2>) -> TokenStream2 {
    match wc {
        Some(w) => quote! { where #w },
        None => quote! {},
    }
}

/// Collect body items from remaining tokens (parenthesized groups → eval_lisp_expr, bare → quoted).
pub(crate) fn parse_body_items(tokens: &[TokenTree], start: usize) -> Vec<LispOutput> {
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
