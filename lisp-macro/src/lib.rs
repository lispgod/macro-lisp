mod debug;
mod output;
mod helpers;
mod shared;
mod expr;
mod forms;
mod items;

use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use proc_macro2::{Delimiter, TokenTree, Span};
use proc_macro_error2::{abort, proc_macro_error};
use quote::{quote, quote_spanned, ToTokens};

use debug::debug_expansion;
use helpers::{flatten_none_delim, is_ident, validate_pattern};
use expr::eval_lisp_expr;
use items::{parse_assign_op, parse_impl, parse_trait, parse_enum, parse_struct, parse_fn};

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
