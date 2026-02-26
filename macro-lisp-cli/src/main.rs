mod codegen;
mod compiler;
mod loader;

use clap::{Parser, Subcommand};
use std::path::{Path, PathBuf};
use std::process;

#[derive(Parser)]
#[command(name = "macro-lisp")]
#[command(about = "Run S-expression (Lisp-like) programs that compile to Rust via macro-lisp")]
#[command(version)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Compile and run a .lisp file
    Run {
        /// Path to the .lisp source file
        file: PathBuf,

        /// Show verbose output (generated Rust, compilation details)
        #[arg(short, long)]
        verbose: bool,

        /// Path to the macro-lisp workspace root (auto-detected if not specified)
        #[arg(long)]
        macro_lisp_path: Option<PathBuf>,
    },

    /// Compile a .lisp file and report any errors (don't run)
    Check {
        /// Path to the .lisp source file
        file: PathBuf,

        /// Show verbose output
        #[arg(short, long)]
        verbose: bool,

        /// Path to the macro-lisp workspace root (auto-detected if not specified)
        #[arg(long)]
        macro_lisp_path: Option<PathBuf>,
    },

    /// Show the generated Rust code for a .lisp file
    Expand {
        /// Path to the .lisp source file
        file: PathBuf,
    },
}

fn main() {
    let cli = Cli::parse();

    let result = match cli.command {
        Commands::Run {
            file,
            verbose,
            macro_lisp_path,
        } => cmd_run(&file, verbose, macro_lisp_path),
        Commands::Check {
            file,
            verbose,
            macro_lisp_path,
        } => cmd_check(&file, verbose, macro_lisp_path),
        Commands::Expand { file } => cmd_expand(&file),
    };

    if let Err(e) = result {
        eprintln!("error: {e}");
        process::exit(1);
    }
}

/// Try to auto-detect the macro-lisp workspace root by looking for Cargo.toml
/// with the `macro_lisp` package name in ancestor directories of the CLI binary.
fn detect_macro_lisp_path(explicit: Option<PathBuf>) -> Option<PathBuf> {
    if let Some(path) = explicit {
        return Some(path);
    }

    // Try the directory of the current executable
    if let Ok(exe) = std::env::current_exe() {
        // Walk up from the exe looking for the workspace root
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

    None
}

fn cmd_run(
    file: &Path,
    verbose: bool,
    macro_lisp_path: Option<PathBuf>,
) -> Result<(), Box<dyn std::error::Error>> {
    if !file.exists() {
        return Err(format!("file not found: {}", file.display()).into());
    }

    // Load and parse the .lisp file
    let exprs = loader::load_file(file)?;

    if exprs.is_empty() {
        eprintln!("warning: no S-expressions found in {}", file.display());
        return Ok(());
    }

    if verbose {
        eprintln!("==> Parsed {} top-level expression(s) from {}", exprs.len(), file.display());
    }

    // Generate Rust source
    let rust_source = codegen::generate_rust_source(&exprs);

    if verbose {
        eprintln!("==> Generated Rust source:");
        eprintln!("---");
        eprintln!("{rust_source}");
        eprintln!("---");
    }

    // Compile
    let macro_lisp_path = detect_macro_lisp_path(macro_lisp_path);

    if verbose {
        if let Some(ref p) = macro_lisp_path {
            eprintln!("==> Using macro-lisp at: {}", p.display());
        } else {
            eprintln!("==> Using macro-lisp from crates.io");
        }
    }

    let work_dir = tempfile::tempdir()?;
    let config = compiler::CompileConfig {
        macro_lisp_path,
        ..Default::default()
    };

    if verbose {
        eprintln!("==> Compiling in: {}", work_dir.path().display());
    }

    let compile_result = compiler::compile(&rust_source, work_dir.path(), &config)?;

    if !compile_result.success {
        compiler::display_diagnostics(&compile_result.diagnostics, &compile_result.raw_stderr);
        return Err("compilation failed".into());
    }

    if verbose {
        eprintln!("==> Compilation successful");
    }

    // Run the binary
    let binary_path = compile_result
        .binary_path
        .ok_or("compilation succeeded but binary not found")?;

    if verbose {
        eprintln!("==> Running: {}", binary_path.display());
        eprintln!("---");
    }

    let run_result = compiler::run_binary(&binary_path)?;

    // Output the program's stdout directly
    if !run_result.stdout.is_empty() {
        print!("{}", run_result.stdout);
    }
    if !run_result.stderr.is_empty() {
        eprint!("{}", run_result.stderr);
    }

    if run_result.exit_code != 0 {
        process::exit(run_result.exit_code);
    }

    Ok(())
}

fn cmd_check(
    file: &Path,
    verbose: bool,
    macro_lisp_path: Option<PathBuf>,
) -> Result<(), Box<dyn std::error::Error>> {
    if !file.exists() {
        return Err(format!("file not found: {}", file.display()).into());
    }

    let exprs = loader::load_file(file)?;

    if exprs.is_empty() {
        eprintln!("warning: no S-expressions found in {}", file.display());
        return Ok(());
    }

    let rust_source = codegen::generate_rust_source(&exprs);

    if verbose {
        eprintln!("==> Generated Rust source:");
        eprintln!("---");
        eprintln!("{rust_source}");
        eprintln!("---");
    }

    let macro_lisp_path = detect_macro_lisp_path(macro_lisp_path);
    let work_dir = tempfile::tempdir()?;
    let config = compiler::CompileConfig {
        macro_lisp_path,
        ..Default::default()
    };

    let compile_result = compiler::compile(&rust_source, work_dir.path(), &config)?;

    if compile_result.success {
        eprintln!("✓ {} compiles successfully ({} expression(s))", file.display(), exprs.len());
        Ok(())
    } else {
        compiler::display_diagnostics(&compile_result.diagnostics, &compile_result.raw_stderr);
        Err("compilation failed".into())
    }
}

fn cmd_expand(file: &Path) -> Result<(), Box<dyn std::error::Error>> {
    if !file.exists() {
        return Err(format!("file not found: {}", file.display()).into());
    }

    let exprs = loader::load_file(file)?;

    if exprs.is_empty() {
        eprintln!("warning: no S-expressions found in {}", file.display());
        return Ok(());
    }

    let rust_source = codegen::generate_rust_source(&exprs);
    print!("{rust_source}");

    Ok(())
}
