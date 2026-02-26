/// Lightweight S-expression loader for the REPL.
///
/// Reuses the same balanced-parenthesis parsing logic as the CLI loader,
/// but operates on in-memory strings rather than files.
use std::path::Path;

/// A parsed top-level S-expression.
#[derive(Debug, Clone)]
pub struct TopLevelExpr {
    pub text: String,
}

/// Check if input has balanced parentheses.
///
/// Returns `true` if all parentheses are balanced, `false` if more input is needed.
/// Respects string literals (parens inside strings don't count).
pub fn is_balanced(input: &str) -> bool {
    let mut depth = 0i32;
    let chars: Vec<char> = input.chars().collect();
    let len = chars.len();
    let mut i = 0;

    while i < len {
        match chars[i] {
            '"' => {
                i += 1;
                while i < len && chars[i] != '"' {
                    if chars[i] == '\\' && i + 1 < len {
                        i += 2;
                    } else {
                        i += 1;
                    }
                }
                if i < len {
                    i += 1; // closing "
                }
            }
            ';' => {
                // Skip line comments
                while i < len && chars[i] != '\n' {
                    i += 1;
                }
            }
            '(' => {
                depth += 1;
                i += 1;
            }
            ')' => {
                depth -= 1;
                i += 1;
            }
            _ => {
                i += 1;
            }
        }
    }

    depth <= 0
}

/// Parse top-level S-expressions from a source string.
///
/// This is used by the `:load` command to load `.lisp` files.
pub fn parse_top_level_exprs(source: &str, _file: &Path) -> Result<Vec<TopLevelExpr>, String> {
    let mut exprs = Vec::new();
    let chars: Vec<char> = source.chars().collect();
    let len = chars.len();
    let mut pos = 0;

    while pos < len {
        // Skip whitespace
        if chars[pos].is_whitespace() {
            pos += 1;
            continue;
        }

        // Skip comments
        if chars[pos] == ';' {
            while pos < len && chars[pos] != '\n' {
                pos += 1;
            }
            continue;
        }

        // Start of a parenthesized expression
        if chars[pos] == '(' {
            let start_pos = pos;
            let mut depth = 0i32;

            loop {
                if pos >= len {
                    return Err("unbalanced parentheses: unexpected end of file".to_string());
                }

                let ch = chars[pos];

                // Handle strings
                if ch == '"' {
                    pos += 1;
                    while pos < len && chars[pos] != '"' {
                        if chars[pos] == '\\' && pos + 1 < len {
                            pos += 2;
                        } else {
                            pos += 1;
                        }
                    }
                    if pos >= len {
                        return Err("unterminated string literal".to_string());
                    }
                    pos += 1;
                    continue;
                }

                if ch == '(' {
                    depth += 1;
                } else if ch == ')' {
                    depth -= 1;
                }

                pos += 1;

                if depth == 0 {
                    break;
                }
            }

            let text: String = chars[start_pos..pos].iter().collect();
            exprs.push(TopLevelExpr { text });
        } else {
            // Non-parenthesized atom
            let start_pos = pos;
            while pos < len
                && !chars[pos].is_whitespace()
                && chars[pos] != '('
                && chars[pos] != ')'
                && chars[pos] != ';'
            {
                pos += 1;
            }
            let text: String = chars[start_pos..pos].iter().collect();
            if !text.is_empty() {
                exprs.push(TopLevelExpr { text });
            }
        }
    }

    Ok(exprs)
}

/// Load and parse a `.lisp` file.
pub fn load_file(path: &Path) -> Result<Vec<TopLevelExpr>, String> {
    let source =
        std::fs::read_to_string(path).map_err(|e| format!("cannot read {}: {e}", path.display()))?;
    parse_top_level_exprs(&source, path)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;

    #[test]
    fn test_is_balanced_simple() {
        assert!(is_balanced("(+ 1 2)"));
        assert!(is_balanced(""));
        assert!(is_balanced("hello"));
    }

    #[test]
    fn test_is_balanced_incomplete() {
        assert!(!is_balanced("(+ 1"));
        assert!(!is_balanced("(fn main () ()"));
        assert!(!is_balanced("(("));
    }

    #[test]
    fn test_is_balanced_with_strings() {
        assert!(is_balanced(r#"(println! "hello (world)")"#));
        assert!(!is_balanced(r#"(println! "hello"#));
    }

    #[test]
    fn test_is_balanced_with_comments() {
        assert!(is_balanced(";; just a comment"));
        assert!(is_balanced("(+ 1 2) ;; trailing comment"));
    }

    #[test]
    fn test_parse_top_level() {
        let exprs = parse_top_level_exprs(
            "(fn add ((a i32) (b i32)) i32 (+ a b))\n(fn main () () (println! \"hi\"))",
            &PathBuf::from("test.lisp"),
        )
        .unwrap();
        assert_eq!(exprs.len(), 2);
        assert!(exprs[0].text.starts_with("(fn add"));
        assert!(exprs[1].text.starts_with("(fn main"));
    }

    #[test]
    fn test_parse_with_comments() {
        let exprs = parse_top_level_exprs(
            ";; comment\n(+ 1 2)\n;; another\n(* 3 4)",
            &PathBuf::from("test.lisp"),
        )
        .unwrap();
        assert_eq!(exprs.len(), 2);
    }
}
