//! Session state for the REPL.
//!
//! Tracks accumulated item definitions (functions, structs, enums, etc.)
//! across interactions so that later expressions can reference earlier definitions.

/// Keywords that indicate a top-level item definition (not an expression).
const ITEM_KEYWORDS: &[&str] = &[
    "fn",
    "pub fn",
    "const fn",
    "pub const fn",
    "async fn",
    "pub async fn",
    "unsafe fn",
    "pub unsafe fn",
    "extern",
    "pub extern",
    "struct",
    "pub struct",
    "pub(crate) struct",
    "enum",
    "pub enum",
    "pub(crate) enum",
    "trait",
    "pub trait",
    "pub(crate) trait",
    "impl",
    "use",
    "pub use",
    "pub(crate) use",
    "type",
    "pub type",
    "pub(crate) type",
    "const",
    "pub const",
    "pub(crate) const",
    "static",
    "pub static",
    "pub(crate) static",
    "mod",
    "pub mod",
    "pub(crate) mod",
    "extern crate",
];

/// Accumulated REPL session state.
#[derive(Debug)]
pub struct Session {
    /// Item definitions accumulated over the session (functions, structs, etc.).
    items: Vec<String>,
    /// Counter for generating unique wrapper function names.
    eval_count: usize,
}

impl Session {
    pub fn new() -> Self {
        Self {
            items: Vec::new(),
            eval_count: 0,
        }
    }

    /// Reset session state — clear all accumulated definitions.
    pub fn reset(&mut self) {
        self.items.clear();
        self.eval_count = 0;
    }

    /// Determine whether an S-expression is an item definition or an expression.
    ///
    /// Items (fn, struct, enum, trait, impl, use, etc.) are accumulated in the preamble.
    /// Expressions (+ 1 2, println!, etc.) are wrapped in a main function for evaluation.
    pub fn is_item(input: &str) -> bool {
        let trimmed = input.trim();
        // Strip outer parens for inspection
        let inner = if trimmed.starts_with('(') && trimmed.ends_with(')') {
            trimmed[1..trimmed.len() - 1].trim()
        } else {
            trimmed
        };

        // Check for attribute prefixes like #[derive(Debug)]
        let check = if inner.starts_with("#[") {
            // Skip past the attribute to find the real keyword
            if let Some(close) = inner.find(']') {
                inner[close + 1..].trim()
            } else {
                inner
            }
        } else {
            inner
        };

        for kw in ITEM_KEYWORDS {
            if let Some(rest) = check.strip_prefix(kw) {
                // Make sure it's a keyword boundary (followed by space or '(')
                if rest.is_empty()
                    || rest.starts_with(' ')
                    || rest.starts_with('(')
                    || rest.starts_with('\n')
                    || rest.starts_with('\t')
                {
                    return true;
                }
            }
        }

        false
    }

    /// Check if an S-expression defines a `fn main`.
    fn is_main_fn(input: &str) -> bool {
        let trimmed = input.trim();
        let inner = if trimmed.starts_with('(') && trimmed.ends_with(')') {
            trimmed[1..trimmed.len() - 1].trim()
        } else {
            trimmed
        };
        inner.starts_with("fn main") || inner.starts_with("pub fn main")
    }

    /// Add an item definition to the session.
    pub fn add_item(&mut self, item: &str) {
        self.items.push(item.to_string());
    }

    /// Generate a complete Rust source file for evaluating an expression.
    ///
    /// The source includes:
    /// - Standard preamble (`use macro_lisp::lisp;`)
    /// - All accumulated item definitions
    /// - A `main()` function that evaluates and prints the expression
    pub fn generate_eval_source(&mut self, expr: &str) -> String {
        self.eval_count += 1;
        let mut out = String::new();

        // Preamble
        out.push_str("#![allow(unused_variables)]\n");
        out.push_str("#![allow(unused_mut)]\n");
        out.push_str("#![allow(unused_assignments)]\n");
        out.push_str("#![allow(dead_code)]\n\n");
        out.push_str("use macro_lisp::lisp;\n\n");

        // Accumulated items (skip any main function — we generate our own)
        for item in &self.items {
            if Self::is_main_fn(item) {
                continue;
            }
            let inner = strip_outer_parens(item);
            out.push_str(&format!("lisp!({inner});\n\n"));
        }

        // Expression wrapper — the expression is evaluated in main() and printed
        let inner_expr = strip_outer_parens(expr);
        out.push_str(&format!(
            "fn main() {{\n    let __repl_result = lisp!({inner_expr});\n    println!(\"{{:?}}\", __repl_result);\n}}\n"
        ));

        out
    }

    /// Generate a complete Rust source file for an item definition.
    ///
    /// This is used to verify that an item compiles before adding it to the session.
    pub fn generate_item_check_source(&self, new_item: &str) -> String {
        let mut out = String::new();

        out.push_str("#![allow(unused_variables)]\n");
        out.push_str("#![allow(unused_mut)]\n");
        out.push_str("#![allow(unused_assignments)]\n");
        out.push_str("#![allow(dead_code)]\n\n");
        out.push_str("use macro_lisp::lisp;\n\n");

        // Accumulated items
        for item in &self.items {
            let inner = strip_outer_parens(item);
            out.push_str(&format!("lisp!({inner});\n\n"));
        }

        // New item to check
        let inner = strip_outer_parens(new_item);
        out.push_str(&format!("lisp!({inner});\n\n"));

        // Only add empty main if no main function was defined
        let has_main = self.items.iter().any(|i| Self::is_main_fn(i)) || Self::is_main_fn(new_item);
        if !has_main {
            out.push_str("fn main() {}\n");
        }

        out
    }

    /// Generate the expanded Rust source for a given expression (for :expand).
    pub fn generate_expand_source(&self, input: &str) -> String {
        let inner = strip_outer_parens(input);
        let mut out = String::new();

        out.push_str("use macro_lisp::lisp;\n\n");
        for item in &self.items {
            let item_inner = strip_outer_parens(item);
            out.push_str(&format!("lisp!({item_inner});\n\n"));
        }

        if Self::is_item(input) {
            out.push_str(&format!("lisp!({inner});\n"));
        } else {
            out.push_str(&format!(
                "fn main() {{\n    let __result = lisp!({inner});\n    println!(\"{{:?}}\", __result);\n}}\n"
            ));
        }

        out
    }
}

/// Strip outer parentheses from an S-expression string.
fn strip_outer_parens(text: &str) -> &str {
    let trimmed = text.trim();
    if trimmed.starts_with('(') && trimmed.ends_with(')') {
        &trimmed[1..trimmed.len() - 1]
    } else {
        trimmed
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_item_fn() {
        assert!(Session::is_item("(fn add ((a i32) (b i32)) i32 (+ a b))"));
        assert!(Session::is_item(
            "(pub fn add ((a i32) (b i32)) i32 (+ a b))"
        ));
        assert!(Session::is_item("(const fn zero () i32 0)"));
    }

    #[test]
    fn test_is_item_struct_enum() {
        assert!(Session::is_item("(struct Point ((x f64) (y f64)))"));
        assert!(Session::is_item("(pub struct Point ((x f64) (y f64)))"));
        assert!(Session::is_item("(enum Color (Red) (Green) (Blue))"));
        assert!(Session::is_item("(pub enum Color (Red) (Green) (Blue))"));
    }

    #[test]
    fn test_is_item_trait_impl() {
        assert!(Session::is_item("(trait Drawable (fn draw (&self)))"));
        assert!(Session::is_item(
            "(impl Display for Point (fn fmt ...))"
        ));
    }

    #[test]
    fn test_is_item_use() {
        assert!(Session::is_item("(use std::fmt)"));
        assert!(Session::is_item("(pub use crate::types)"));
    }

    #[test]
    fn test_is_item_type_const_static() {
        assert!(Session::is_item("(type Pair (i32 i32))"));
        assert!(Session::is_item("(const PI f64 3.14)"));
        assert!(Session::is_item("(static GLOBAL i32 0)"));
    }

    #[test]
    fn test_is_expression() {
        assert!(!Session::is_item("(+ 1 2)"));
        assert!(!Session::is_item("(println! \"hello\")"));
        assert!(!Session::is_item("(* 3 4)"));
        assert!(!Session::is_item("(if (> x 0) 1 0)"));
        assert!(!Session::is_item("(let x 42)"));
        assert!(!Session::is_item("(block (let x 1) (+ x 2))"));
    }

    #[test]
    fn test_session_accumulates_items() {
        let mut session = Session::new();
        session.add_item("(fn double ((x i32)) i32 (* x 2))");

        let source = session.generate_eval_source("(double 21)");
        assert!(source.contains("lisp!(fn double"));
        assert!(source.contains("lisp!(double 21)"));
    }

    #[test]
    fn test_session_reset() {
        let mut session = Session::new();
        session.add_item("(fn foo () i32 42)");
        assert_eq!(session.items.len(), 1);

        session.reset();
        assert_eq!(session.items.len(), 0);
    }

    #[test]
    fn test_strip_outer_parens() {
        assert_eq!(strip_outer_parens("(+ 1 2)"), "+ 1 2");
        assert_eq!(strip_outer_parens("atom"), "atom");
        assert_eq!(
            strip_outer_parens("(fn main () ())"),
            "fn main () ()"
        );
    }

    #[test]
    fn test_generate_eval_source_structure() {
        let mut session = Session::new();
        let source = session.generate_eval_source("(+ 1 2)");

        assert!(source.contains("use macro_lisp::lisp;"));
        assert!(source.contains("fn main()"));
        assert!(source.contains("lisp!(+ 1 2)"));
        assert!(source.contains("println!"));
    }

    #[test]
    fn test_generate_item_check_source() {
        let mut session = Session::new();
        session.add_item("(fn foo () i32 42)");

        let source = session.generate_item_check_source("(fn bar () i32 (foo))");
        assert!(source.contains("lisp!(fn foo"));
        assert!(source.contains("lisp!(fn bar"));
        assert!(source.contains("fn main() {}"));
    }

    #[test]
    fn test_generate_expand_source() {
        let session = Session::new();

        // Expression
        let source = session.generate_expand_source("(+ 1 2)");
        assert!(source.contains("fn main()"));

        // Item
        let source = session.generate_expand_source("(fn add ((a i32) (b i32)) i32 (+ a b))");
        assert!(source.contains("lisp!(fn add"));
        assert!(!source.contains("fn main()"));
    }
}
