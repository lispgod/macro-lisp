mod commands;
mod compiler;
mod loader;
mod session;

use commands::ReplCommand;
use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;
use session::Session;
use std::path::PathBuf;

const VERSION: &str = env!("CARGO_PKG_VERSION");

fn main() {
    let macro_lisp_path = detect_macro_lisp_path();

    eprintln!("macro-lisp REPL v{VERSION}");
    eprintln!("Type :help for available commands, :quit to exit.");
    eprintln!();

    if let Err(e) = run_repl(macro_lisp_path) {
        eprintln!("REPL error: {e}");
        std::process::exit(1);
    }
}

fn run_repl(macro_lisp_path: Option<PathBuf>) -> std::result::Result<(), Box<dyn std::error::Error>> {
    let mut rl = DefaultEditor::new()?;
    let mut session = Session::new();

    // Set up the persistent work directory for incremental compilation
    let work_dir = compiler::setup_work_dir(&macro_lisp_path)?;

    // Try to load history
    let history_path = dirs_history_path();
    if let Some(ref hp) = history_path {
        let _ = rl.load_history(hp);
    }

    let mut input_buffer = String::new();
    let mut continuation = false;

    loop {
        let prompt = if continuation { "...> " } else { "λ> " };

        match rl.readline(prompt) {
            Ok(line) => {
                let trimmed = line.trim();

                // Handle empty input
                if trimmed.is_empty() && !continuation {
                    continue;
                }

                // If we're in continuation mode, append to buffer
                if continuation {
                    input_buffer.push('\n');
                    input_buffer.push_str(&line);
                } else {
                    input_buffer = line.clone();
                }

                // Check if parentheses are balanced
                if !loader::is_balanced(&input_buffer) {
                    continuation = true;
                    continue;
                }

                // We have a complete input — reset continuation
                continuation = false;
                let input = input_buffer.trim().to_string();
                input_buffer.clear();

                if input.is_empty() {
                    continue;
                }

                // Add to history
                let _ = rl.add_history_entry(&input);

                // Check for REPL commands
                if let Some(cmd) = commands::parse_command(&input) {
                    match cmd {
                        ReplCommand::Help => {
                            commands::print_help();
                        }
                        ReplCommand::Quit => {
                            eprintln!("Goodbye!");
                            break;
                        }
                        ReplCommand::Expand(expr) => {
                            if expr.is_empty() {
                                eprintln!("Usage: :expand <expression>");
                            } else {
                                handle_expand(&session, &expr);
                            }
                        }
                        ReplCommand::Reset => {
                            session.reset();
                            eprintln!("Session reset — all definitions cleared.");
                        }
                        ReplCommand::Load(path) => {
                            if path.is_empty() {
                                eprintln!("Usage: :load <file.lisp>");
                            } else {
                                handle_load(&mut session, &path, &work_dir);
                            }
                        }
                        ReplCommand::Unknown(cmd) => {
                            eprintln!("Unknown command: :{cmd}");
                            eprintln!("Type :help for available commands.");
                        }
                    }
                    continue;
                }

                // Evaluate the input
                handle_eval(&mut session, &input, &work_dir);
            }
            Err(ReadlineError::Interrupted) => {
                // Ctrl-C: cancel current input
                if continuation {
                    eprintln!("Input cancelled.");
                    input_buffer.clear();
                    continuation = false;
                } else {
                    eprintln!("Type :quit to exit.");
                }
            }
            Err(ReadlineError::Eof) => {
                // Ctrl-D: exit
                eprintln!("Goodbye!");
                break;
            }
            Err(err) => {
                eprintln!("Error: {err}");
                break;
            }
        }
    }

    // Save history
    if let Some(ref hp) = history_path {
        let _ = rl.save_history(hp);
    }

    Ok(())
}

/// Evaluate an S-expression input.
fn handle_eval(session: &mut Session, input: &str, work_dir: &std::path::Path) {
    if Session::is_item(input) {
        // Item definition — check if it compiles, then add to session
        let source = session.generate_item_check_source(input);

        match compiler::compile(&source, work_dir) {
            Ok(result) => {
                if result.success {
                    session.add_item(input);
                    // Silently accept items — no output unless there's an error
                } else {
                    eprintln!("{}", result.raw_stderr);
                }
            }
            Err(e) => {
                eprintln!("Compilation error: {e}");
            }
        }
    } else {
        // Expression — evaluate and print the result
        let source = session.generate_eval_source(input);

        match compiler::compile(&source, work_dir) {
            Ok(result) => {
                if result.success {
                    if let Some(ref binary) = result.binary_path {
                        match compiler::run_binary(binary) {
                            Ok(run) => {
                                if !run.stdout.is_empty() {
                                    print!("{}", run.stdout);
                                }
                                if !run.stderr.is_empty() {
                                    eprint!("{}", run.stderr);
                                }
                            }
                            Err(e) => {
                                eprintln!("Runtime error: {e}");
                            }
                        }
                    } else {
                        eprintln!("Compilation succeeded but binary not found.");
                    }
                } else {
                    eprintln!("{}", result.raw_stderr);
                }
            }
            Err(e) => {
                eprintln!("Compilation error: {e}");
            }
        }
    }
}

/// Handle the :expand command.
fn handle_expand(session: &Session, expr: &str) {
    let source = session.generate_expand_source(expr);
    println!("{source}");
}

/// Handle the :load command.
fn handle_load(session: &mut Session, path: &str, work_dir: &std::path::Path) {
    let file_path = std::path::Path::new(path);
    match loader::load_file(file_path) {
        Ok(exprs) => {
            if exprs.is_empty() {
                eprintln!("No S-expressions found in {path}");
                return;
            }

            let mut loaded = 0;
            for expr in &exprs {
                if Session::is_item(&expr.text) {
                    // Try to add items
                    let source = session.generate_item_check_source(&expr.text);
                    match compiler::compile(&source, work_dir) {
                        Ok(result) => {
                            if result.success {
                                session.add_item(&expr.text);
                                loaded += 1;
                            } else {
                                eprintln!("Error loading item: {}", expr.text.chars().take(60).collect::<String>());
                                eprintln!("{}", result.raw_stderr);
                            }
                        }
                        Err(e) => {
                            eprintln!("Compilation error: {e}");
                        }
                    }
                } else {
                    // Evaluate expressions
                    handle_eval(session, &expr.text, work_dir);
                    loaded += 1;
                }
            }

            eprintln!("Loaded {loaded} expression(s) from {path}");
        }
        Err(e) => {
            eprintln!("Error loading {path}: {e}");
        }
    }
}

/// Try to auto-detect the macro-lisp workspace root.
fn detect_macro_lisp_path() -> Option<PathBuf> {
    // Try current working directory and ancestors
    if let Ok(cwd) = std::env::current_dir() {
        let mut dir = Some(cwd);
        while let Some(ref d) = dir {
            let cargo_toml = d.join("Cargo.toml");
            if cargo_toml.exists() {
                if let Ok(contents) = std::fs::read_to_string(&cargo_toml) {
                    if contents.contains("name = \"macro_lisp\"") {
                        return Some(d.clone());
                    }
                }
            }
            dir = d.parent().map(|p| p.to_path_buf());
        }
    }

    // Try from the executable path
    if let Ok(exe) = std::env::current_exe() {
        let mut dir = exe.parent().map(|p| p.to_path_buf());
        while let Some(ref d) = dir {
            let cargo_toml = d.join("Cargo.toml");
            if cargo_toml.exists() {
                if let Ok(contents) = std::fs::read_to_string(&cargo_toml) {
                    if contents.contains("name = \"macro_lisp\"") {
                        return Some(d.clone());
                    }
                }
            }
            dir = d.parent().map(|p| p.to_path_buf());
        }
    }

    None
}

/// Get the path for REPL history file.
fn dirs_history_path() -> Option<PathBuf> {
    // Use temp dir for history since we can't guarantee home dir access
    let dir = std::env::temp_dir().join("macro-lisp-repl");
    let _ = std::fs::create_dir_all(&dir);
    Some(dir.join("history.txt"))
}
