# macro-lisp Roadmap

A detailed plan for improving, testing, and extending `macro-lisp` — from better test coverage and error handling to a fully interactive REPL with LSP support.

---

## Phase 1: Testing & Quality Foundation

Strengthen the existing macro system with comprehensive testing before building new features.

### 1.1 Expand Unit & Integration Test Coverage

- **Goal:** Achieve thorough coverage of every S-expression form documented in `REFERENCE.md`.
- **Tasks:**
  - Audit `REFERENCE.md` against the existing test files (`tests/core.rs`, `tests/operators.rs`, etc.) and add missing test cases for any undocumented or under-tested forms.
  - Add edge-case tests for each operator and special form:
    - Empty argument lists (e.g., `(+)`, `(block)`).
    - Deeply nested expressions (e.g., 10+ levels of nesting).
    - Unusual but valid Rust types in generics, lifetimes, and where clauses.
  - Add negative tests (`tests/errors/`) for every form that should produce a compile error, verifying error message quality using `trybuild`.

### 1.2 Property-Based & Fuzz Testing

- **Goal:** Discover edge cases that hand-written tests miss.
- **Tasks:**
  - Introduce [`proptest`](https://crates.io/crates/proptest) or [`quickcheck`](https://crates.io/crates/quickcheck) as a dev-dependency.
  - Write generators for valid S-expression token streams (atoms, nested lists, operators, keywords).
  - Property: every generated valid S-expression should either expand to compilable Rust or produce a clear error (never panic, never produce invalid tokens silently).
  - Integrate a fuzz target (`cargo-fuzz`) that feeds arbitrary byte sequences to the proc macro entry points, checking for panics.

### 1.3 Snapshot / Golden-File Testing for Macro Expansion

- **Goal:** Catch unintended changes to macro output.
- **Tasks:**
  - Use the existing `debug.rs` module (or `cargo expand`) to capture the Rust code each `lisp!(...)` invocation expands to.
  - Store these expansions as golden files (e.g., `tests/snapshots/*.rs.expanded`).
  - Add a CI step that regenerates snapshots and diffs against committed versions, failing if they diverge unexpectedly.

### 1.4 CI Hardening

- **Goal:** Ensure every PR is fully validated.
- **Tasks:**
  - Add a `cargo test --examples` step (already present — verify examples stay up to date).
  - Add MSRV (Minimum Supported Rust Version) testing to CI (e.g., Rust 1.70).
  - Add `cargo deny` or `cargo audit` for supply-chain safety.
  - Consider `miri` testing for any `unsafe` code paths.

---

## Phase 2: S-Expression File Loader & Compiler Driver

Build a standalone binary that reads `.lisp` source files, wraps them in `lisp!()`, compiles them with `rustc`, and reports results.

### 2.1 File Format & Parser

- **Goal:** Define and parse a `.lisp` file format that can hold top-level S-expressions.
- **File format:**
  ```lisp
  ;; example.lisp
  (fn factorial ((n i32)) i32
    (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

  (fn main () ()
    (let result (factorial 10))
    (println! "10! = {}" result))
  ```
- **Tasks:**
  - Write a lightweight front-end parser that:
    1. Reads a `.lisp` file from disk.
    2. Splits it into top-level S-expressions (balanced-parentheses splitting, respecting strings and comments).
    3. Preserves source location information (byte offset, line, column) for every token and parenthesized group.
  - Store parsed expressions in an intermediate representation (IR) that retains source spans:
    ```rust
    struct SourceSpan {
        file: PathBuf,
        start_line: usize,
        start_col: usize,
        end_line: usize,
        end_col: usize,
    }

    enum SExpr {
        Atom { value: String, span: SourceSpan },
        List { children: Vec<SExpr>, span: SourceSpan },
    }
    ```

### 2.2 Code Generation: Wrapping in `lisp!()`

- **Goal:** Transform parsed S-expressions into a compilable Rust source file.
- **Tasks:**
  - Generate a temporary `.rs` file containing:
    ```rust
    use macro_lisp::lisp;

    lisp!(
        // ... each top-level S-expression from the .lisp file ...
    );
    ```
  - Maintain a **source map** that maps byte ranges in the generated `.rs` file back to the original `.lisp` file spans. This is critical for Phase 5 (error handling).
  - Support a `--emit-rust` flag that writes the generated `.rs` to stdout or a file for inspection, even before compilation.

### 2.3 Compilation via `rustc`

- **Goal:** Compile the generated Rust source and capture results.
- **Approach — use `rustc` as a library or as a subprocess:**
  - **Option A (subprocess — recommended for v1):** Invoke `rustc` (or `cargo`) as a child process on the generated `.rs` file. Parse JSON-formatted diagnostics from stderr.
  - **Option B (library — future):** Use the `rustc_driver` and `rustc_interface` crates to invoke the compiler programmatically. This provides deeper integration (e.g., access to the AST after macro expansion) but depends on nightly Rust and unstable APIs.
- **Tasks:**
  - Implement Option A first:
    1. Write the generated `.rs` to a temp directory.
    2. Invoke `rustc --edition 2021 --error-format=json -L <path-to-macro-lisp> <generated>.rs`.
    3. Capture stdout (compiled binary path) and stderr (diagnostics JSON).
    4. Parse `rustc` JSON diagnostics into structured error objects.
  - If compilation succeeds, run the resulting binary and capture its stdout/stderr.
  - Display the output to the user.

### 2.4 CLI Interface

- **Goal:** A polished command-line tool for running `.lisp` files.
- **Usage:**
  ```
  macro-lisp run example.lisp          # compile and run
  macro-lisp check example.lisp        # compile only, report errors
  macro-lisp expand example.lisp       # show generated Rust code
  macro-lisp fmt example.lisp          # (future) format S-expressions
  ```
- **Tasks:**
  - Create a new binary crate in the workspace (e.g., `macro-lisp-cli/`).
  - Use [`clap`](https://crates.io/crates/clap) for argument parsing.
  - Wire up the file loader (2.1), code generator (2.2), and compiler driver (2.3).
  - Add `--verbose` flag for debugging (show generated Rust, `rustc` invocation, timing).

---

## Phase 3: Interactive REPL

Build an interactive read-eval-print loop for live S-expression evaluation.

### 3.1 Core REPL Loop

- **Goal:** Users type S-expressions, see the result immediately.
- **Design:**
  ```
  λ> (+ 1 2)
  3
  λ> (let x 42)
  λ> (* x x)
  1764
  ```
- **Tasks:**
  - Use [`rustyline`](https://crates.io/crates/rustyline) for line editing, history, and multi-line input detection (balanced parentheses).
  - Determine expression vs. item context:
    - **Expressions** (e.g., `(+ 1 2)`) get wrapped in a `fn main()` that prints the result.
    - **Items** (e.g., `(fn foo ...)`, `(struct Bar ...)`) get accumulated into a persistent preamble.
  - Maintain a session state that accumulates definitions across interactions:
    ```
    λ> (fn double ((x i32)) i32 (* x 2))
    λ> (double 21)
    42
    ```

### 3.2 Incremental Compilation

- **Goal:** Fast feedback — sub-second response times for simple expressions.
- **Tasks:**
  - Cache previously compiled preamble code; only recompile when new items are added.
  - Use `cargo` with incremental compilation enabled in a persistent temp project.
  - Explore using `rustc_driver` (nightly) for in-process compilation to avoid process-spawn overhead.
  - Pre-compile the `macro_lisp` dependency once, reuse the compiled artifacts for all REPL evaluations.

### 3.3 Rich REPL Features

- **Goal:** A pleasant interactive experience.
- **Tasks:**
  - **Parenthesis matching:** Highlight matching parens as the user types.
  - **Multi-line editing:** Detect unbalanced parentheses and prompt for continuation (`...>`).
  - **`:commands`:**
    - `:expand <expr>` — show the Rust code an expression expands to.
    - `:type <expr>` — (future) infer and display the type of an expression.
    - `:reset` — clear session state.
    - `:load <file>` — load and evaluate a `.lisp` file.
    - `:help` — list available commands.
    - `:quit` — exit.
  - **Syntax highlighting** of S-expressions in the terminal (using ANSI colors via [`colored`](https://crates.io/crates/colored) or similar).
  - **Tab completion** for known identifiers (built-in forms, user-defined names).

---

## Phase 4: LSP (Language Server Protocol) Support

Provide IDE integration for `.lisp` files via the Language Server Protocol.

### 4.1 Language Server Skeleton

- **Goal:** A working LSP server that editors can connect to.
- **Tasks:**
  - Create a new binary crate `macro-lisp-lsp/`.
  - Use [`tower-lsp`](https://crates.io/crates/tower-lsp) or [`lsp-server`](https://crates.io/crates/lsp-server) as the LSP framework.
  - Register the server for `.lisp` file extensions.
  - Implement the core LSP lifecycle: `initialize`, `initialized`, `shutdown`, `exit`.

### 4.2 Diagnostics (Errors & Warnings)

- **Goal:** Show compilation errors inline in the editor, mapped back to the `.lisp` source.
- **Tasks:**
  - On file save (or on every keystroke with debouncing), run the file through the compile pipeline (Phase 2).
  - Map `rustc` diagnostics back to `.lisp` source spans using the source map from Phase 2.2.
  - Publish diagnostics via `textDocument/publishDiagnostics`.
  - Include both:
    1. The **S-expression span** that caused the error (primary location).
    2. The **generated Rust code** as a related information snippet for debugging.

### 4.3 Syntax Highlighting (Semantic Tokens)

- **Goal:** Rich, accurate syntax coloring in editors.
- **Tasks:**
  - Implement `textDocument/semanticTokens/full`.
  - Classify tokens: keywords (`fn`, `let`, `if`, `match`, `struct`, `enum`, `trait`, `impl`, etc.), operators (`+`, `-`, `*`, `==`, etc.), literals (numbers, strings, chars), comments, identifiers, types.
  - Provide a TextMate grammar (`.tmLanguage.json`) as a fallback for editors that don't support semantic tokens. Distribute this as a VS Code extension.

### 4.4 Hover & Go-to-Definition

- **Goal:** Show information about symbols on hover; jump to definitions.
- **Tasks:**
  - **Hover:** Show the expanded Rust code for the S-expression under the cursor.
  - **Go-to-definition:** For user-defined functions, structs, etc., jump to their definition in the `.lisp` file.
  - **Document symbols:** Provide an outline of all top-level definitions (functions, structs, enums, traits) in the file.

### 4.5 Completion

- **Goal:** Suggest valid completions as the user types.
- **Tasks:**
  - Complete special form keywords (`fn`, `let`, `if`, `match`, `for`, `while`, `loop`, `struct`, `enum`, `trait`, `impl`, `use`, etc.).
  - Complete user-defined identifiers (functions, variables, types) from the current file.
  - Complete field names after `.` access.
  - (Future) integrate with `rust-analyzer` for Rust-level completions on the expanded code.

### 4.6 VS Code Extension

- **Goal:** First-class editing experience in VS Code.
- **Tasks:**
  - Create a VS Code extension that:
    1. Bundles the LSP server binary.
    2. Registers `.lisp` file association.
    3. Provides a TextMate grammar for basic highlighting.
    4. Starts the LSP server on activation.
  - Add commands: "Expand to Rust", "Run File", "Open REPL".

---

## Phase 5: Error Handling & Diagnostics with Spans

The crown jewel — rich, context-aware error messages that bridge the gap between S-expression source and generated Rust.

### 5.1 Source Span Tracking in the Proc Macro

- **Goal:** Preserve original source locations through macro expansion.
- **Tasks:**
  - Audit `lisp-macro/src/expr.rs`, `forms.rs`, `items.rs` to ensure every generated `TokenStream` uses `Span` information from the corresponding input tokens.
  - Where `Span::call_site()` is currently used, switch to the span of the relevant input token so `rustc` errors point to the right place in the `lisp!(...)` invocation.
  - Use `Span::join()` (nightly) or manual span tracking to produce spans that cover entire S-expressions, not just individual tokens.

### 5.2 Dual-View Error Display

- **Goal:** When a compilation error occurs, show the user:
  1. The **S-expression source** with the error location highlighted.
  2. The **expanded Rust code** with the error location highlighted.
  3. The `rustc` error message.
- **Example output:**
  ```
  error[E0308]: mismatched types
    --> example.lisp:3:5
     |
   3 |     (+ "hello" 2)
     |     ^^^^^^^^^^^^^^ expected integer, found `&str`
     |
     = expanded Rust:
       |
       |   "hello" + 2
       |   ^^^^^^^ expected integer, found `&str`
     |
     = help: the S-expression `(+ "hello" 2)` expands to `"hello" + 2`
  ```
- **Tasks:**
  - Extend the diagnostic structures to carry both the `.lisp` span and the generated `.rs` span.
  - Format errors using [`ariadne`](https://crates.io/crates/ariadne) or [`miette`](https://crates.io/crates/miette) for beautiful terminal output with:
    - Colored source snippets.
    - Underlines and carets pointing to the exact error location.
    - Multi-file support (show both the `.lisp` source and the generated `.rs` source).
  - In the LSP (Phase 4), send `DiagnosticRelatedInformation` linking to the generated Rust code.

### 5.3 Macro Expansion Debugging

- **Goal:** Let users inspect what their S-expressions compile to at any granularity.
- **Tasks:**
  - Add an `--expand` flag to the CLI that shows the full expanded Rust output.
  - Add a `--expand-step` flag that shows expansion step-by-step for each top-level form:
    ```
    ;; Input:
    (fn add ((a i32) (b i32)) i32 (+ a b))

    ;; Expands to:
    fn add(a: i32, b: i32) -> i32 {
        a + b
    }
    ```
  - In the REPL, the `:expand` command provides this interactively.
  - In the LSP, hover shows the expansion for the form under the cursor.

### 5.4 Error Recovery & Partial Compilation

- **Goal:** Don't abort on the first error — report as many errors as possible.
- **Tasks:**
  - In the file loader (Phase 2), if one top-level S-expression fails to parse, report the error and continue parsing the rest.
  - When compiling, pass errors through and accumulate all diagnostics before presenting them.
  - In the REPL, an error in one expression should not corrupt the session state.

### 5.5 User-Facing Error Catalog

- **Goal:** Provide actionable, documented error messages.
- **Tasks:**
  - Assign error codes to common mistakes (e.g., `ML001: unbalanced parentheses`, `ML002: unknown special form`, `ML003: wrong number of arguments`).
  - Write a documentation page explaining each error code with examples and fixes.
  - Include the error code in every diagnostic so users can look it up.

---

## Milestone Summary

| Milestone | Key Deliverable | Dependencies |
|---|---|---|
| **Phase 1** | Comprehensive tests, CI hardened | None |
| **Phase 2** | `macro-lisp run example.lisp` works | Phase 1 |
| **Phase 3** | Interactive REPL with history & `:expand` | Phase 2 |
| **Phase 4** | VS Code extension with LSP diagnostics | Phase 2, Phase 5.1–5.2 |
| **Phase 5** | Dual-view errors with source spans | Phase 2 |

Phases 2 and 5 can be developed in parallel. Phase 3 builds on Phase 2. Phase 4 builds on Phases 2 and 5.

---

## Suggested Crate / Workspace Layout

```
macro-lisp/
├── lisp-macro/           # proc macro crate (existing)
├── lisp/                 # public library crate (existing)
├── macro-lisp-cli/       # CLI binary: run, check, expand, fmt
│   ├── Cargo.toml
│   └── src/
│       ├── main.rs       # clap entry point
│       ├── loader.rs     # .lisp file parser
│       ├── codegen.rs    # wrap in lisp!(), generate .rs
│       ├── compiler.rs   # invoke rustc, parse diagnostics
│       ├── sourcemap.rs  # .lisp ↔ .rs span mapping
│       └── display.rs    # ariadne/miette error rendering
├── macro-lisp-repl/      # REPL binary
│   ├── Cargo.toml
│   └── src/
│       ├── main.rs       # rustyline loop
│       ├── session.rs    # accumulated definitions state
│       └── commands.rs   # :expand, :type, :reset, etc.
├── macro-lisp-lsp/       # Language server binary
│   ├── Cargo.toml
│   └── src/
│       ├── main.rs       # LSP server entry
│       ├── diagnostics.rs
│       ├── semantic_tokens.rs
│       ├── hover.rs
│       └── completion.rs
├── editors/
│   └── vscode/           # VS Code extension
│       ├── package.json
│       ├── syntaxes/macro-lisp.tmLanguage.json
│       └── src/extension.ts
├── tests/                # existing + new test files
├── examples/             # existing + new examples
├── Cargo.toml            # workspace root
├── ROADMAP.md            # this file
├── REFERENCE.md          # syntax reference
└── README.md             # getting started
```

---

## Contributing

Contributions are welcome at every phase. Each phase is designed to be independently useful:

- **Phase 1** makes the existing macro more reliable.
- **Phase 2** unlocks running `.lisp` files as standalone programs.
- **Phase 3** provides an interactive playground.
- **Phase 4** brings IDE-quality editing.
- **Phase 5** makes errors a joy to debug rather than a chore.

Pick any phase or sub-task and open a PR!
