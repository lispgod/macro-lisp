use serde::Deserialize;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::Command;

/// Result of a compilation attempt.
#[derive(Debug)]
pub struct CompileResult {
    pub success: bool,
    pub binary_path: Option<PathBuf>,
    pub diagnostics: Vec<Diagnostic>,
    pub raw_stderr: String,
}

/// A single diagnostic message from rustc.
#[derive(Debug, Clone)]
#[allow(dead_code)]
pub struct Diagnostic {
    pub level: DiagnosticLevel,
    pub message: String,
    pub rendered: Option<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum DiagnosticLevel {
    Error,
    Warning,
    Note,
    Help,
    Other(String),
}

/// Raw JSON diagnostic from `rustc --error-format=json`.
#[derive(Deserialize)]
struct RustcDiagnostic {
    message: Option<String>,
    level: Option<String>,
    rendered: Option<String>,
}

/// Configuration for compilation.
#[derive(Default)]
pub struct CompileConfig {
    /// Path to the macro-lisp workspace root (for local path dependency).
    pub macro_lisp_path: Option<PathBuf>,
    /// Build in release mode.
    pub release: bool,
}

/// Create a temporary Cargo project, write the generated Rust source,
/// compile it, and return the result.
pub fn compile(
    rust_source: &str,
    work_dir: &Path,
    config: &CompileConfig,
) -> Result<CompileResult, Box<dyn std::error::Error>> {
    // Create directory structure
    let src_dir = work_dir.join("src");
    std::fs::create_dir_all(&src_dir)?;

    // Write Cargo.toml
    let cargo_toml = generate_cargo_toml(config);
    std::fs::write(work_dir.join("Cargo.toml"), cargo_toml)?;

    // Write the generated Rust source as main.rs
    std::fs::write(src_dir.join("main.rs"), rust_source)?;

    // First, try building with JSON output for structured diagnostics
    let mut json_cmd = Command::new("cargo");
    json_cmd
        .arg("build")
        .arg("--message-format=json")
        .current_dir(work_dir);

    if config.release {
        json_cmd.arg("--release");
    }

    let json_output = json_cmd.output()?;
    let json_stdout = String::from_utf8_lossy(&json_output.stdout).to_string();

    // Parse JSON diagnostics for structured data
    let diagnostics = parse_diagnostics(&json_stdout);

    // Also capture human-readable stderr by doing a second build (cached, instant)
    // if there were errors, using the regular error format
    let raw_stderr = if !json_output.status.success() {
        let mut stderr_cmd = Command::new("cargo");
        stderr_cmd.arg("build").current_dir(work_dir);
        if config.release {
            stderr_cmd.arg("--release");
        }
        let stderr_output = stderr_cmd.output()?;
        String::from_utf8_lossy(&stderr_output.stderr).to_string()
    } else {
        String::new()
    };

    let binary_path = if json_output.status.success() {
        let profile = if config.release { "release" } else { "debug" };
        let binary = work_dir
            .join("target")
            .join(profile)
            .join("macro-lisp-script");

        if binary.exists() {
            Some(binary)
        } else {
            // Try platform-specific names
            let binary_exe = work_dir
                .join("target")
                .join(profile)
                .join("macro-lisp-script.exe");
            if binary_exe.exists() {
                Some(binary_exe)
            } else {
                None
            }
        }
    } else {
        None
    };

    Ok(CompileResult {
        success: json_output.status.success(),
        binary_path,
        diagnostics,
        raw_stderr,
    })
}

/// Run a compiled binary and capture its output.
pub fn run_binary(binary_path: &Path) -> Result<RunResult, Box<dyn std::error::Error>> {
    let output = Command::new(binary_path).output()?;

    Ok(RunResult {
        exit_code: output.status.code().unwrap_or(-1),
        stdout: String::from_utf8_lossy(&output.stdout).to_string(),
        stderr: String::from_utf8_lossy(&output.stderr).to_string(),
    })
}

/// Result of running a compiled binary.
#[derive(Debug)]
pub struct RunResult {
    pub exit_code: i32,
    pub stdout: String,
    pub stderr: String,
}

/// Generate a `Cargo.toml` for the temporary project.
fn generate_cargo_toml(config: &CompileConfig) -> String {
    let dep = if let Some(ref path) = config.macro_lisp_path {
        // Use local path dependency — point to the `lisp` crate directory which
        // re-exports the proc macros, just like the real macro_lisp depends on it.
        // We also need the root macro_lisp crate.
        let path_str = path.display();
        format!("macro_lisp = {{ path = \"{path_str}\" }}")
    } else {
        // Fallback to crates.io
        "macro_lisp = \"0.2\"".to_string()
    };

    format!(
        r#"[package]
name = "macro-lisp-script"
version = "0.0.0"
edition = "2021"
publish = false

[dependencies]
{dep}

[workspace]
"#
    )
}

/// Parse diagnostics from cargo JSON output.
fn parse_diagnostics(json_output: &str) -> Vec<Diagnostic> {
    let mut diagnostics = Vec::new();

    for line in json_output.lines() {
        let trimmed = line.trim();
        if !trimmed.starts_with('{') {
            continue;
        }

        if let Ok(msg) = serde_json::from_str::<RustcDiagnostic>(trimmed) {
            if let Some(message) = msg.message {
                let level = match msg.level.as_deref() {
                    Some("error") => DiagnosticLevel::Error,
                    Some("warning") => DiagnosticLevel::Warning,
                    Some("note") => DiagnosticLevel::Note,
                    Some("help") => DiagnosticLevel::Help,
                    Some(other) => DiagnosticLevel::Other(other.to_string()),
                    None => DiagnosticLevel::Other("unknown".to_string()),
                };
                diagnostics.push(Diagnostic {
                    level,
                    message,
                    rendered: msg.rendered,
                });
            }
        }
    }

    diagnostics
}

/// Display diagnostics to stderr in a user-friendly format.
pub fn display_diagnostics(diagnostics: &[Diagnostic], raw_stderr: &str) {
    let has_rendered = diagnostics.iter().any(|d| d.rendered.is_some());

    if has_rendered {
        // Use rustc's own rendered output — it's already well-formatted
        for diag in diagnostics {
            if let Some(ref rendered) = diag.rendered {
                eprint!("{rendered}");
            }
        }
    } else if !raw_stderr.trim().is_empty() {
        // Fall back to raw stderr (human-readable cargo/rustc output)
        // Filter out cargo progress/metadata lines
        for line in raw_stderr.lines() {
            let trimmed = line.trim();
            if trimmed.is_empty() {
                continue;
            }
            // Keep error/warning lines, filter build progress
            if trimmed.starts_with("Compiling")
                || trimmed.starts_with("Downloading")
                || trimmed.starts_with("Downloaded")
                || trimmed.starts_with("Updating")
                || trimmed.starts_with("Locking")
                || trimmed.starts_with("Finished")
                || trimmed.starts_with("Blocking")
            {
                continue;
            }
            let _ = writeln!(std::io::stderr(), "{line}");
        }
    }
}
