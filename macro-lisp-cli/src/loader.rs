use std::path::{Path, PathBuf};

/// Source location information for a token or S-expression.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceSpan {
    pub file: PathBuf,
    pub start_line: usize,
    pub start_col: usize,
    pub end_line: usize,
    pub end_col: usize,
}

impl std::fmt::Display for SourceSpan {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}:{}:{}",
            self.file.display(),
            self.start_line,
            self.start_col,
        )
    }
}

/// A parsed S-expression with source span information.
#[derive(Debug, Clone)]
pub struct TopLevelExpr {
    /// The raw text of the S-expression (including outer parentheses for lists).
    pub text: String,
    /// Source location of the expression.
    pub span: SourceSpan,
}

/// Errors that can occur during loading.
#[derive(Debug)]
pub enum LoadError {
    Io(std::io::Error),
    Parse {
        message: String,
        file: PathBuf,
        line: usize,
        col: usize,
    },
}

impl std::fmt::Display for LoadError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LoadError::Io(e) => write!(f, "I/O error: {e}"),
            LoadError::Parse {
                message,
                file,
                line,
                col,
            } => write!(f, "{}:{line}:{col}: {message}", file.display()),
        }
    }
}

impl std::error::Error for LoadError {}

impl From<std::io::Error> for LoadError {
    fn from(e: std::io::Error) -> Self {
        LoadError::Io(e)
    }
}

/// Load a `.lisp` file from disk and split it into top-level S-expressions.
///
/// Top-level expressions are either:
/// - Parenthesized lists `(...)` at the top level
/// - Single atoms at the top level (rare but supported)
///
/// Comments starting with `;;` are stripped. Strings are respected
/// (parentheses inside strings don't count).
pub fn load_file(path: &Path) -> Result<Vec<TopLevelExpr>, LoadError> {
    let source = std::fs::read_to_string(path)?;
    parse_top_level_exprs(&source, path)
}

/// Parse source text into top-level S-expressions.
pub fn parse_top_level_exprs(source: &str, file: &Path) -> Result<Vec<TopLevelExpr>, LoadError> {
    let mut exprs = Vec::new();
    let chars: Vec<char> = source.chars().collect();
    let len = chars.len();
    let mut pos = 0;
    let mut line = 1usize;
    let mut col = 1usize;

    while pos < len {
        // Skip whitespace
        if chars[pos].is_whitespace() {
            advance(&chars, &mut pos, &mut line, &mut col, 1);
            continue;
        }

        // Skip line comments: ;; or ;
        if chars[pos] == ';' {
            while pos < len && chars[pos] != '\n' {
                pos += 1;
                col += 1;
            }
            continue;
        }

        // Start of a parenthesized expression
        if chars[pos] == '(' {
            let start_line = line;
            let start_col = col;
            let start_pos = pos;
            let mut depth = 0i32;

            loop {
                if pos >= len {
                    return Err(LoadError::Parse {
                        message: "unbalanced parentheses: unexpected end of file".into(),
                        file: file.to_path_buf(),
                        line: start_line,
                        col: start_col,
                    });
                }

                let ch = chars[pos];

                // Handle strings — skip their contents
                if ch == '"' {
                    advance(&chars, &mut pos, &mut line, &mut col, 1);
                    while pos < len && chars[pos] != '"' {
                        if chars[pos] == '\\' && pos + 1 < len {
                            // Skip escaped character
                            advance(&chars, &mut pos, &mut line, &mut col, 2);
                        } else {
                            advance(&chars, &mut pos, &mut line, &mut col, 1);
                        }
                    }
                    if pos >= len {
                        return Err(LoadError::Parse {
                            message: "unterminated string literal".into(),
                            file: file.to_path_buf(),
                            line,
                            col,
                        });
                    }
                    advance(&chars, &mut pos, &mut line, &mut col, 1); // closing "
                    continue;
                }

                // Handle char literals
                if ch == '\'' && depth > 0 {
                    // Could be a char literal like 'c' or a lifetime like 'a
                    // Simple heuristic: if next char after quote is followed by another quote, it's a char
                    if pos + 2 < len && chars[pos + 2] == '\'' {
                        advance(&chars, &mut pos, &mut line, &mut col, 3);
                        continue;
                    }
                    // Otherwise just advance past it
                    advance(&chars, &mut pos, &mut line, &mut col, 1);
                    continue;
                }

                if ch == '(' {
                    depth += 1;
                } else if ch == ')' {
                    depth -= 1;
                }

                advance(&chars, &mut pos, &mut line, &mut col, 1);

                if depth == 0 {
                    break;
                }
            }

            let text: String = chars[start_pos..pos].iter().collect();
            exprs.push(TopLevelExpr {
                text,
                span: SourceSpan {
                    file: file.to_path_buf(),
                    start_line,
                    start_col,
                    end_line: line,
                    end_col: col,
                },
            });
        } else {
            // Non-parenthesized top-level token (an atom)
            let start_line = line;
            let start_col = col;
            let start_pos = pos;

            while pos < len
                && !chars[pos].is_whitespace()
                && chars[pos] != '('
                && chars[pos] != ')'
                && chars[pos] != ';'
            {
                advance(&chars, &mut pos, &mut line, &mut col, 1);
            }

            let text: String = chars[start_pos..pos].iter().collect();
            if !text.is_empty() {
                exprs.push(TopLevelExpr {
                    text,
                    span: SourceSpan {
                        file: file.to_path_buf(),
                        start_line,
                        start_col,
                        end_line: line,
                        end_col: col,
                    },
                });
            }
        }
    }

    Ok(exprs)
}

/// Advance position by `count` characters, updating line/col tracking.
fn advance(chars: &[char], pos: &mut usize, line: &mut usize, col: &mut usize, count: usize) {
    for _ in 0..count {
        if *pos < chars.len() {
            if chars[*pos] == '\n' {
                *line += 1;
                *col = 1;
            } else {
                *col += 1;
            }
            *pos += 1;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;

    fn parse(src: &str) -> Vec<TopLevelExpr> {
        parse_top_level_exprs(src, &PathBuf::from("test.lisp")).unwrap()
    }

    #[test]
    fn test_single_expression() {
        let exprs = parse("(+ 1 2)");
        assert_eq!(exprs.len(), 1);
        assert_eq!(exprs[0].text, "(+ 1 2)");
        assert_eq!(exprs[0].span.start_line, 1);
        assert_eq!(exprs[0].span.start_col, 1);
    }

    #[test]
    fn test_multiple_expressions() {
        let src =
            "(fn add ((a i32) (b i32)) i32\n  (+ a b))\n\n(fn main () ()\n  (println! \"hello\"))";
        let exprs = parse(src);
        assert_eq!(exprs.len(), 2);
        assert!(exprs[0].text.starts_with("(fn add"));
        assert!(exprs[1].text.starts_with("(fn main"));
    }

    #[test]
    fn test_comments_stripped() {
        let src = ";; This is a comment\n(+ 1 2)\n;; Another comment\n(* 3 4)";
        let exprs = parse(src);
        assert_eq!(exprs.len(), 2);
        assert_eq!(exprs[0].text, "(+ 1 2)");
        assert_eq!(exprs[1].text, "(* 3 4)");
    }

    #[test]
    fn test_string_with_parens() {
        let src = "(println! \"hello (world)\")";
        let exprs = parse(src);
        assert_eq!(exprs.len(), 1);
        assert_eq!(exprs[0].text, "(println! \"hello (world)\")");
    }

    #[test]
    fn test_nested_expressions() {
        let src = "(if (> x 0) (println! \"positive\") (println! \"non-positive\"))";
        let exprs = parse(src);
        assert_eq!(exprs.len(), 1);
        assert_eq!(exprs[0].text, src);
    }

    #[test]
    fn test_unbalanced_parens() {
        let result = parse_top_level_exprs("(+ 1 2", &PathBuf::from("test.lisp"));
        assert!(result.is_err());
    }

    #[test]
    fn test_span_tracking() {
        let src = "(fn a () ())\n\n(fn b () ())";
        let exprs = parse(src);
        assert_eq!(exprs.len(), 2);
        assert_eq!(exprs[0].span.start_line, 1);
        assert_eq!(exprs[1].span.start_line, 3);
    }

    #[test]
    fn test_empty_source() {
        let exprs = parse("");
        assert_eq!(exprs.len(), 0);
    }

    #[test]
    fn test_only_comments() {
        let exprs = parse(";; just a comment\n;; and another");
        assert_eq!(exprs.len(), 0);
    }

    #[test]
    fn test_escaped_string() {
        let src = r#"(println! "she said \"hi\"")"#;
        let exprs = parse(src);
        assert_eq!(exprs.len(), 1);
    }
}
