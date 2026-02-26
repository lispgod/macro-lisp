/// Compilation driver for the REPL.
///
/// Creates a temporary Cargo project, writes generated Rust source,
/// compiles it, and optionally runs the result.
use serde::Deserialize;
use std::path::{Path, PathBuf};
use std::process::Command;

/// Result of a compilation attempt.
#[derive(Debug)]
pub struct CompileResult {
    pub success: bool,
    pub binary_path: Option<PathBuf>,
    pub raw_stderr: String,
}

/// Result of running a compiled binary.
#[derive(Debug)]
pub struct RunResult {
    #[allow(dead_code)]
    pub exit_code: i32,
    pub stdout: String,
    pub stderr: String,
}

/// Raw JSON diagnostic from `rustc --error-format=json`.
#[derive(Deserialize)]
struct RustcDiagnostic {
    rendered: Option<String>,
}

/// Compile the generated Rust source in a work directory.
///
/// The work directory should already contain a valid Cargo.toml.
/// This function writes `src/main.rs` and invokes `cargo build`.
pub fn compile(
    rust_source: &str,
    work_dir: &Path,
) -> Result<CompileResult, Box<dyn std::error::Error>> {
    let src_dir = work_dir.join("src");
    std::fs::create_dir_all(&src_dir)?;
    std::fs::write(src_dir.join("main.rs"), rust_source)?;

    // Build with JSON output for structured diagnostics
    let json_output = Command::new("cargo")
        .arg("build")
        .arg("--message-format=json")
        .current_dir(work_dir)
        .output()?;

    // Extract rendered errors from JSON output
    let raw_stderr = if !json_output.status.success() {
        extract_rendered_errors(&String::from_utf8_lossy(&json_output.stdout))
    } else {
        String::new()
    };

    let binary_path = if json_output.status.success() {
        let binary = work_dir
            .join("target")
            .join("debug")
            .join("macro-lisp-repl-eval");
        if binary.exists() {
            Some(binary)
        } else {
            None
        }
    } else {
        None
    };

    Ok(CompileResult {
        success: json_output.status.success(),
        binary_path,
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

/// Generate a Cargo.toml for the REPL eval project.
pub fn generate_cargo_toml(macro_lisp_path: &Option<PathBuf>) -> String {
    let dep = if let Some(ref path) = macro_lisp_path {
        let path_str = path.display();
        format!("macro_lisp = {{ path = \"{path_str}\" }}")
    } else {
        "macro_lisp = \"0.2\"".to_string()
    };

    format!(
        r#"[package]
name = "macro-lisp-repl-eval"
version = "0.0.0"
edition = "2021"
publish = false

[dependencies]
{dep}

[workspace]
"#
    )
}

/// Set up a persistent work directory for REPL compilation.
///
/// Uses a stable path so incremental compilation can cache artifacts.
pub fn setup_work_dir(
    macro_lisp_path: &Option<PathBuf>,
) -> Result<PathBuf, Box<dyn std::error::Error>> {
    let work_dir = std::env::temp_dir().join("macro-lisp-repl-workspace");
    let src_dir = work_dir.join("src");
    std::fs::create_dir_all(&src_dir)?;

    let cargo_toml = generate_cargo_toml(macro_lisp_path);
    std::fs::write(work_dir.join("Cargo.toml"), cargo_toml)?;

    Ok(work_dir)
}

/// Extract rendered error messages from cargo JSON output.
fn extract_rendered_errors(json_output: &str) -> String {
    let mut rendered = String::new();

    for line in json_output.lines() {
        let trimmed = line.trim();
        if !trimmed.starts_with('{') {
            continue;
        }
        if let Ok(msg) = serde_json::from_str::<RustcDiagnostic>(trimmed) {
            if let Some(r) = msg.rendered {
                rendered.push_str(&r);
            }
        }
    }

    if rendered.is_empty() {
        // Fall back to raw stderr-style output
        json_output
            .lines()
            .filter(|l| {
                let t = l.trim();
                !t.is_empty()
                    && !t.starts_with('{')
                    && !t.starts_with("Compiling")
                    && !t.starts_with("Downloading")
                    && !t.starts_with("Downloaded")
                    && !t.starts_with("Updating")
                    && !t.starts_with("Locking")
                    && !t.starts_with("Finished")
                    && !t.starts_with("Blocking")
            })
            .collect::<Vec<_>>()
            .join("\n")
    } else {
        rendered
    }
}
