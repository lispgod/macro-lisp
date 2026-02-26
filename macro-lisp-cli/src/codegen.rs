use crate::loader::TopLevelExpr;

/// Generate a complete Rust source file from parsed S-expressions.
///
/// The generated file wraps each top-level expression in a `lisp!()` invocation
/// and includes the `use macro_lisp::lisp;` import.
pub fn generate_rust_source(exprs: &[TopLevelExpr]) -> String {
    let mut out = String::new();

    // Standard preamble
    out.push_str("#![allow(unused_variables)]\n");
    out.push_str("#![allow(unused_mut)]\n");
    out.push_str("#![allow(unused_assignments)]\n");
    out.push_str("#![allow(dead_code)]\n\n");
    out.push_str("use macro_lisp::lisp;\n\n");

    for expr in exprs {
        // Strip the outer parentheses for the lisp!() invocation
        // since lisp!() expects the inner tokens.
        let inner = strip_outer_parens(&expr.text);

        out.push_str(&format!(
            "// source: {}:{}\n",
            expr.span.file.display(),
            expr.span.start_line
        ));
        out.push_str(&format!("lisp!({inner});\n\n"));
    }

    out
}

/// Strip outer parentheses from an S-expression string.
///
/// `(fn main () ())` → `fn main () ()`
///
/// If the text is not wrapped in parens, return it as-is.
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
    use crate::loader::{SourceSpan, TopLevelExpr};
    use std::path::PathBuf;

    fn make_expr(text: &str, line: usize) -> TopLevelExpr {
        TopLevelExpr {
            text: text.to_string(),
            span: SourceSpan {
                file: PathBuf::from("test.lisp"),
                start_line: line,
                start_col: 1,
                end_line: line,
                end_col: text.len() + 1,
            },
        }
    }

    #[test]
    fn test_strip_outer_parens() {
        assert_eq!(strip_outer_parens("(+ 1 2)"), "+ 1 2");
        assert_eq!(strip_outer_parens("(fn main () ())"), "fn main () ()");
        assert_eq!(strip_outer_parens("atom"), "atom");
    }

    #[test]
    fn test_generate_basic() {
        let exprs = vec![make_expr("(fn main () () (println! \"hello\"))", 1)];
        let source = generate_rust_source(&exprs);
        assert!(source.contains("use macro_lisp::lisp;"));
        assert!(source.contains("lisp!(fn main () () (println! \"hello\"));"));
    }

    #[test]
    fn test_generate_multiple() {
        let exprs = vec![
            make_expr("(fn add ((a i32) (b i32)) i32 (+ a b))", 1),
            make_expr("(fn main () () (println! \"{}\" (add 1 2)))", 4),
        ];
        let source = generate_rust_source(&exprs);
        // Should contain both lisp! invocations
        assert!(source.contains("lisp!(fn add"));
        assert!(source.contains("lisp!(fn main"));
    }

    #[test]
    fn test_generate_includes_source_comments() {
        let exprs = vec![make_expr("(+ 1 2)", 42)];
        let source = generate_rust_source(&exprs);
        assert!(source.contains("// source: test.lisp:42"));
    }
}
