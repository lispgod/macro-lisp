//! REPL commands (prefixed with `:` in interactive mode).
//!
//! Supported commands:
//! - `:help` — list available commands
//! - `:quit` / `:q` — exit the REPL
//! - `:expand <expr>` — show the generated Rust code
//! - `:reset` — clear session state
//! - `:load <file>` — load and evaluate a `.lisp` file

/// A parsed REPL command.
#[derive(Debug, PartialEq)]
pub enum ReplCommand {
    Help,
    Quit,
    Expand(String),
    Reset,
    Load(String),
    Unknown(String),
}

/// Parse a REPL command from user input.
///
/// Returns `None` if the input is not a command (doesn't start with `:`).
pub fn parse_command(input: &str) -> Option<ReplCommand> {
    let trimmed = input.trim();
    if !trimmed.starts_with(':') {
        return None;
    }

    let without_colon = trimmed[1..].trim();
    let (cmd, rest) = match without_colon.find(char::is_whitespace) {
        Some(idx) => (&without_colon[..idx], without_colon[idx..].trim()),
        None => (without_colon, ""),
    };

    match cmd {
        "help" | "h" | "?" => Some(ReplCommand::Help),
        "quit" | "q" | "exit" => Some(ReplCommand::Quit),
        "expand" | "e" => {
            if rest.is_empty() {
                Some(ReplCommand::Expand(String::new()))
            } else {
                Some(ReplCommand::Expand(rest.to_string()))
            }
        }
        "reset" | "clear" => Some(ReplCommand::Reset),
        "load" | "l" => {
            if rest.is_empty() {
                Some(ReplCommand::Load(String::new()))
            } else {
                Some(ReplCommand::Load(rest.to_string()))
            }
        }
        _ => Some(ReplCommand::Unknown(cmd.to_string())),
    }
}

/// Print the help message.
pub fn print_help() {
    eprintln!("Available commands:");
    eprintln!("  :help, :h, :?         Show this help message");
    eprintln!("  :quit, :q, :exit      Exit the REPL");
    eprintln!("  :expand <expr>, :e    Show the generated Rust code for an expression");
    eprintln!("  :reset, :clear        Clear all accumulated definitions");
    eprintln!("  :load <file>, :l      Load and evaluate a .lisp file");
    eprintln!();
    eprintln!("Enter S-expressions directly to evaluate them:");
    eprintln!("  λ> (+ 1 2)");
    eprintln!("  3");
    eprintln!();
    eprintln!("Define functions and types that persist across evaluations:");
    eprintln!("  λ> (fn double ((x i32)) i32 (* x 2))");
    eprintln!("  λ> (double 21)");
    eprintln!("  42");
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_help() {
        assert_eq!(parse_command(":help"), Some(ReplCommand::Help));
        assert_eq!(parse_command(":h"), Some(ReplCommand::Help));
        assert_eq!(parse_command(":?"), Some(ReplCommand::Help));
    }

    #[test]
    fn test_parse_quit() {
        assert_eq!(parse_command(":quit"), Some(ReplCommand::Quit));
        assert_eq!(parse_command(":q"), Some(ReplCommand::Quit));
        assert_eq!(parse_command(":exit"), Some(ReplCommand::Quit));
    }

    #[test]
    fn test_parse_expand() {
        assert_eq!(
            parse_command(":expand (+ 1 2)"),
            Some(ReplCommand::Expand("(+ 1 2)".to_string()))
        );
        assert_eq!(
            parse_command(":e (fn foo () i32 42)"),
            Some(ReplCommand::Expand("(fn foo () i32 42)".to_string()))
        );
        assert_eq!(
            parse_command(":expand"),
            Some(ReplCommand::Expand(String::new()))
        );
    }

    #[test]
    fn test_parse_reset() {
        assert_eq!(parse_command(":reset"), Some(ReplCommand::Reset));
        assert_eq!(parse_command(":clear"), Some(ReplCommand::Reset));
    }

    #[test]
    fn test_parse_load() {
        assert_eq!(
            parse_command(":load scripts/hello.lisp"),
            Some(ReplCommand::Load("scripts/hello.lisp".to_string()))
        );
        assert_eq!(
            parse_command(":l test.lisp"),
            Some(ReplCommand::Load("test.lisp".to_string()))
        );
    }

    #[test]
    fn test_parse_unknown() {
        assert_eq!(
            parse_command(":foobar"),
            Some(ReplCommand::Unknown("foobar".to_string()))
        );
    }

    #[test]
    fn test_not_a_command() {
        assert_eq!(parse_command("(+ 1 2)"), None);
        assert_eq!(parse_command("hello"), None);
    }
}
