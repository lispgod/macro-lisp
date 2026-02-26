use proc_macro2::TokenStream as TokenStream2;
use quote::ToTokens;

// ─── LispOutput: typed output from S-expression evaluation ────────────────────
// Represents the result of evaluating an S-expression, distinguishing between
// expressions, statements, items, and raw token fallbacks.

pub(crate) enum LispOutput {
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
    pub(crate) fn into_expr(self) -> syn::Expr {
        match self {
            LispOutput::Expr(e) => e,
            other => syn::Expr::Verbatim(other.to_token_stream()),
        }
    }
}

// ─── Helpers for syn AST construction ─────────────────────────────────────────

/// Create a syn::Expr from raw tokens (fallback for complex forms).
pub(crate) fn verbatim_expr(ts: TokenStream2) -> syn::Expr {
    syn::Expr::Verbatim(ts)
}

/// Wrap an expression in parentheses to preserve precedence.
pub(crate) fn paren_wrap(expr: syn::Expr) -> syn::Expr {
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
pub(crate) fn build_block_stmts(items: Vec<LispOutput>) -> Vec<syn::Stmt> {
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
