# Architecture Analysis: `macro_rules!` vs `proc-macro`

This document analyzes the two macro implementation strategies used in
**macro-lisp**, identifies duplication, and proposes a path toward a cleaner,
more powerful design that is closely integrated with `syn` and `quote`.

---

## 1. Current Crate Layout

```
macro_lisp   (root)          ← public façade; re-exports from `lisp`
├── lisp/    (macro_rules!)  ← `lisp!` entry point + thin dispatch macros
└── lisp-macro/ (proc-macro) ← `#[proc_macro]` helpers called by `lisp!`
```

| Crate | Mechanism | Exported Symbols |
|---|---|---|
| `lisp` | `macro_rules!` | `lisp!`, `lisp_arg!`, `lisp_match_arg!`, `lisp_dot_call!`, `lisp_dot_literal!`, `lisp_match_dispatch!` |
| `lisp-macro` | `proc_macro` (with `syn`, `quote`, `proc-macro-error2`) | `lisp_fn!`, `lisp_struct!`, `lisp_enum!`, `lisp_trait!`, `lisp_impl!`, `lisp_assign!`, `lisp_let!` |
| `macro_lisp` | re-export | All of the above via `pub use` |

### How they interact

1. A user writes `lisp!(...)`.
2. The `macro_rules! lisp` in `lisp/src/lib.rs` pattern-matches the input.
3. Simple forms (arithmetic, comparison, boolean, `if`, `while`, `for`, `let`,
   assignment, closures, collections, etc.) are **expanded inline** by
   `macro_rules!`.
4. Complex forms (named functions, structs, enums, traits, impl blocks,
   pattern-destructuring `let`, and arbitrary-LHS assignment) are **delegated**
   to the corresponding `#[proc_macro]` in `lisp-macro`.

---

## 2. What `macro_rules!` Handles Today (in `/lisp`)

The `lisp!` macro has roughly **80 pattern arms** organized into these groups:

| Category | Example Form | Notes |
|---|---|---|
| Literals | `(true)`, `(false)`, `(self.x.y)` | Trivial identity transforms |
| Item dispatch | `struct`, `enum`, `trait`, `impl`, `fn` (named) | **Delegates** to proc macros after `$vis:vis` capture |
| `type`, `const`, `static` | `(const X i32 = 5)` | Handled inline; simple token rearrangement |
| `match` | `(match e (pat => body)…)` | Mostly inline; one arm falls back to `lisp_match_dispatch!` TT-muncher |
| `let` / `let mut` | typed, untyped, scoped, else | Inline for simple forms; **delegates** to `lisp_let!` for pattern destructuring |
| Assignment (`=`, `+=`, …) | `(= var val)`, `(+= self.x 1)` | Simple `$var:ident` inline; complex LHS **delegates** to `lisp_assign!` |
| Closures | `(fn ((x i32)) (body))`, `(fn move …)` | Fully inline via `macro_rules!` |
| Loops / Control | `loop`, `while`, `for`, `if`, `break`, `continue`, `return`, `block` | Fully inline |
| Operators | `+`, `-`, `*`, `/`, `%`, `==`, `!=`, `<`, `>`, `&&`, `\|\|`, `!`, etc. | Fully inline; variadic via recursive expansion |
| References / cast | `ref`, `deref`, `as`, `?`, `await` | Fully inline |
| Collections | `tuple`, `vec`, `array` | Fully inline |
| Construction | `new Name (field val)…` | Fully inline |
| Field / index | `(. obj field)`, `(index coll key)` | Inline + helper macros (`lisp_dot_call!`, `lisp_dot_literal!`) |
| Macro invocation | `(name! args)`, `(macro ! name args)` | Fully inline |
| Catch-all | `($sym $args*)` → function call | Fully inline |
| Error | `($($t:tt)*)` → `compile_error!` | Inline |

**Supporting helper macros** (also `macro_rules!`):

| Macro | Purpose |
|---|---|
| `lisp_arg!` | Unwrap a single parenthesized group or pass-through expression |
| `lisp_match_arg!` | Like `lisp_arg!` but also handles `name!` macro invocations |
| `lisp_dot_call!` | Chained method-call expansion (`obj.m1.m2 args`) |
| `lisp_dot_literal!` | Workaround for `.` followed by integer literal (tuple index) |
| `lisp_match_dispatch!` | TT-muncher for match arms with mixed guards |

---

## 3. What the Proc Macro Handles Today (in `/lisp-macro`)

The `lisp-macro` crate (~1980 lines) provides seven `#[proc_macro]` entry
points and a rich internal evaluator:

| Proc Macro | Purpose | `syn`/`quote` usage |
|---|---|---|
| `lisp_fn!` | Named function definitions | `syn::Type`, `syn::Pat`, `syn::Visibility`, `syn::Generics`; `quote!`, `quote_spanned!` |
| `lisp_struct!` | Struct definitions (named, tuple, unit) | `syn::Type`; `quote!` |
| `lisp_enum!` | Enum definitions (unit, tuple, struct variants) | `syn::Type`; `quote!` |
| `lisp_trait!` | Trait definitions with method signatures | `syn::Visibility`, `syn::Type`; `quote!` |
| `lisp_impl!` | Inherent and trait impl blocks | `syn::Type`, `syn::Generics`; `quote!` |
| `lisp_assign!` | Arbitrary-LHS assignment (`self.x += 1`) | `quote!`, `quote_spanned!` |
| `lisp_let!` | Pattern-destructuring `let` bindings | `syn::Pat` (multi-pattern); `quote!`, `quote_spanned!` |

### Internal evaluator (`eval_lisp_expr` and helpers)

The proc-macro crate contains its own **mini S-expression evaluator**
(`eval_lisp_expr`, ~420 lines) that handles the same forms as `lisp!`:
`new`, `if`, `cond`, `let`, `match`, `return`, `break`, `continue`, `ref`,
`deref`, `as`, `for`, `while`, `loop`, `block`, `index`, `tuple`, `vec`,
`array`, `val`, `await`, `rust`, field access, method calls, path calls,
macro invocations, all punctuation-based operators, etc.

This evaluator exists because **function bodies** (inside `lisp_fn!`,
`lisp_impl!`, `lisp_trait!`) need to recursively transform S-expressions,
and calling back to `macro_rules! lisp!` from a proc macro creates
hygiene and scoping issues (especially with `self`).

---

## 4. Duplication Analysis

### 4.1 High duplication — expression evaluation

The core S-expression → Rust transformation logic is implemented **twice**:

| Feature | `macro_rules! lisp!` (lisp/) | `eval_lisp_expr` (lisp-macro/) |
|---|---|---|
| Arithmetic (`+`, `-`, `*`, `/`, `%`) | ✅ variadic recursive arms | ✅ `eval_variadic_op` |
| Comparison (`==`, `!=`, `<`, `>`, `<=`, `>=`) | ✅ inline arms | ✅ `eval_binary_op` |
| Logical (`&&`, `\|\|`, `!`) | ✅ inline arms | ✅ inline match |
| Bitwise (`&`, `\|`, `^`, `<<`, `>>`) | ✅ variadic recursive arms | ✅ `eval_variadic_op` |
| Ranges (`..`, `..=`) | ✅ inline arms | ✅ `eval_punct_expr` |
| Assignment (`=`, `+=`, …) | ✅ inline + delegate | ✅ `eval_compound_assign` |
| `if` / `cond` | ✅ inline arms | ✅ `eval_if`, `eval_cond` |
| `let` / `let mut` | ✅ inline arms | ✅ `eval_let` |
| `for` / `while` / `loop` / `block` | ✅ inline arms | ✅ `eval_for`, `eval_while`, inline |
| `break`, `continue`, `return` | ✅ inline arms | ✅ inline match |
| `ref`, `deref`, `as`, `?`, `await` | ✅ inline arms | ✅ inline match |
| `new` (struct construction) | ✅ inline arms | ✅ inline match |
| `tuple`, `vec`, `array` | ✅ inline arms | ✅ inline match |
| `val`, `self`, `rust` | ✅ inline arms | ✅ inline match |
| `index`, `.` (field access) | ✅ inline arms | ✅ inline match |
| Macro invocation (`name!`) | ✅ inline arms | ✅ inline match |
| Function/method calls | ✅ catch-all arms | ✅ ident/path dispatch |

**Every expression form is duplicated.** This is the single largest source of
maintenance burden and potential for divergence.

### 4.2 Moderate duplication — fn signature parsing

Function signatures are parsed in the proc macro via `parse_fn_signature`
(shared between `lisp_fn!` and `lisp_impl!` body items). The `macro_rules!`
side merely captures `$vis:vis` and qualifier keywords, then forwards
everything to the proc macro. This is **well-factored** today — no
significant duplication for fn signatures.

### 4.3 Low duplication — item definitions

`struct`, `enum`, `trait`, and `impl` parsing lives exclusively in the
proc-macro crate. The `macro_rules!` side just does lightweight dispatch
(capture `$vis:vis`, `$name:ident`, `$(#[$m:meta])*`, then delegate).
This is also **well-factored**.

### 4.4 Summary

| Layer | Lines (approx.) | Duplicated? |
|---|---|---|
| `lisp!` pattern arms | ~370 | **Yes** — expression evaluation |
| `lisp_arg!`, helper macros | ~50 | **Yes** — `eval_lisp_arg` mirrors `lisp_arg!` |
| `lisp_match_dispatch!` | ~20 | Unique to `macro_rules!` (TT-muncher) |
| `lisp_fn!` / `parse_fn` | ~200 | No — authoritative implementation |
| `lisp_struct!` / `parse_struct` | ~120 | No — authoritative implementation |
| `lisp_enum!` / `parse_enum` | ~100 | No — authoritative implementation |
| `lisp_trait!` / `parse_trait` | ~80 | No — authoritative implementation |
| `lisp_impl!` / `parse_impl` | ~80 | No — authoritative implementation |
| `eval_lisp_expr` + helpers | ~600 | **Yes** — mirrors `lisp!` arms |
| `lisp_assign!` / `lisp_let!` | ~120 | Fallback only — not full duplication |

**Estimated duplication:** ~600–800 lines of conceptually identical logic are
maintained in both `macro_rules!` and `proc-macro` form.

---

## 5. Should We Move More from `/lisp` to `/lisp-macro`?

### 5.1 What to move

**Yes — the entire expression evaluator should be unified in the proc macro.**

The `eval_lisp_expr` function in `lisp-macro` already handles every expression
form that `lisp!` handles. Today the proc macro uses this evaluator only for
function/method bodies inside `lisp_fn!`, `lisp_impl!`, and `lisp_trait!`.
By exposing a single **`lisp!` proc macro** that calls `eval_lisp_expr`
for *all* input, we could eliminate the duplicated `macro_rules!` arms.

Specifically, these categories could move to the proc macro:

- All arithmetic, comparison, logical, and bitwise operators
- All control flow (`if`, `cond`, `match`, `for`, `while`, `loop`, `block`)
- All binding forms (`let`, `let mut`, assignment)
- All reference/cast/try operators
- All construction (`new`, `tuple`, `vec`, `array`)
- All field/index access
- All macro invocations and function/method calls
- `return`, `break`, `continue`, `unsafe` blocks, `await`

### 5.2 What to keep in `macro_rules!`

A thin `macro_rules! lisp!` wrapper is still valuable as the **public entry
point** for two reasons:

1. **Ergonomic `$vis:vis` capture.** The `$vis:vis` fragment specifier
   (which matches `pub`, `pub(crate)`, `pub(super)`, or nothing) is only
   available in `macro_rules!`. Proc macros must parse visibility manually.
   The current approach of letting `macro_rules!` capture `$vis:vis` before
   delegating works well.

2. **`$pat:pat_param`-based match.** The `match` form benefits from
   `macro_rules!` pattern fragment specifiers (`$pattern:pat_param`) which
   allow seamless matching of Rust patterns including `|` alternatives.
   Reproducing this in a proc macro requires more manual parsing.

However, both of these can be handled in the proc macro with `syn` parsers
(`syn::Visibility`, `syn::Pat::parse_multi`), which the crate already uses.
The tradeoff is a small amount of manual parsing vs. eliminating hundreds of
lines of duplicated logic.

### 5.3 What NOT to move

- **`lisp_arg!`** should remain as a `macro_rules!` macro (or become a very
  thin proc-macro wrapper). It's used pervasively in user-facing expansions
  and its simplicity (one match arm for groups, one for expressions) doesn't
  warrant a proc macro.

- **`lisp_dot_literal!`** exists solely to work around a Rust macro
  limitation with `.` followed by integer literals. If `lisp!` moves to a
  proc macro, this workaround becomes unnecessary (proc macros can emit
  `tuple.0` directly via `quote!`).

- **`lisp_match_dispatch!`** is a TT-muncher for mixed guard/non-guard match
  arms. If match parsing moves to the proc macro, this helper is eliminated.

---

## 6. Deduplication Strategy

### 6.1 Recommended architecture (target state)

```
macro_lisp   (root)             ← pub use lisp::lisp;
├── lisp/    (macro_rules!)     ← THIN lisp! entry: delegates to lisp_eval!
└── lisp-macro/ (proc-macro)    ← lisp_eval!: the ONE authoritative evaluator
                                   + lisp_fn!, lisp_struct!, lisp_enum!,
                                     lisp_trait!, lisp_impl!, lisp_assign!,
                                     lisp_let!
```

#### Step 1: Expose a `lisp_eval!` proc macro

Create a new `#[proc_macro]` (or rename the existing entry points) that
accepts arbitrary S-expression tokens and transforms them to Rust using the
existing `eval_lisp_expr` engine:

```rust
// In lisp-macro/src/lib.rs
#[proc_macro]
#[proc_macro_error]
pub fn lisp_eval(input: TokenStream) -> TokenStream {
    let tokens: Vec<TokenTree> = flatten_none_delim(
        TokenStream2::from(input).into_iter().collect()
    );
    let result = eval_lisp_expr(&tokens);
    debug_expansion("lisp_eval!", &result);
    result.into()
}
```

#### Step 2: Simplify `lisp!` to a thin dispatcher

```rust
// In lisp/src/lib.rs  (drastically simplified)
#[macro_export]
macro_rules! lisp {
    // Item definitions — capture $vis:vis then delegate
    ( $(#[$m:meta])* $vis:vis struct $name:ident $($rest:tt)* ) => {
        $crate::lisp_struct!($(#[$m])* $vis struct $name $($rest)*);
    };
    ( $(#[$m:meta])* $vis:vis enum $name:ident $($rest:tt)* ) => {
        $crate::lisp_enum!($(#[$m])* $vis enum $name $($rest)*);
    };
    // ... other item forms that benefit from $vis:vis ...

    // Everything else → proc macro evaluator
    ( $($tokens:tt)+ ) => {
        $crate::lisp_eval!($($tokens)+)
    };
}
```

This reduces `lisp!` from ~80 arms to ~10, eliminates the duplicated
expression evaluator in `macro_rules!`, and makes `eval_lisp_expr` the
**single source of truth** for all S-expression → Rust transformation.

#### Step 3: Eliminate helper macros

Once expression evaluation is fully in the proc macro:

| Macro | Action |
|---|---|
| `lisp_arg!` | Keep (still useful for user-level `lisp_arg!` calls in expansions); or internalize into proc macro |
| `lisp_match_arg!` | Remove — proc macro handles match arms directly |
| `lisp_dot_call!` | Remove — proc macro emits method chains via `quote!` |
| `lisp_dot_literal!` | Remove — proc macro emits `tuple.0` directly |
| `lisp_match_dispatch!` | Remove — proc macro handles guard-clause match arms |

#### Step 4: Merge `eval_lisp_expr` callback paths

Today, when the proc macro encounters a `match` form inside a function body,
it emits `::lisp::lisp!(match ...)` — calling back to `macro_rules!`. After
Step 2, this callback can simply call `eval_lisp_expr` recursively within the
proc macro itself, which is already the pattern for `if`, `let`, `for`,
`while`, etc.

---

## 7. `syn` and `quote` Integration

### 7.1 Current usage

The proc macro already leverages `syn` and `quote` extensively:

| `syn` feature | Where used |
|---|---|
| `syn::Type` (parse + quote) | `validate_type`, `parse_type_list`, parameter parsing |
| `syn::Pat` (`Pat::parse_multi`) | `validate_pattern` in `lisp_let!` |
| `syn::Visibility` | `parse_visibility` |
| `syn::Generics` | `validate_generics` |
| `syn::parse2::<T>()` | Used throughout for validation |
| `syn::parse::Parse` trait | Custom parsers (`TypeList`) |
| `syn::Error` / `syn::Result` | Structured error handling in all `parse_*` fns |
| `quote!` | Code generation everywhere |
| `quote_spanned!` | Span-preserving output in `lisp_let!`, `lisp_assign!` |
| `proc_macro_error2::abort!` | Early-exit error reporting |

### 7.2 Opportunities for deeper `syn` integration

#### A. Parse full expressions with `syn::Expr`

Currently, `eval_lisp_expr` manually walks token trees. For sub-expressions
that are already valid Rust (e.g. the RHS of assignments, function arguments,
match scrutinees), we could **parse them with `syn::Expr`** and re-emit:

```rust
fn try_as_rust_expr(tokens: &[TokenTree]) -> Option<syn::Expr> {
    let ts: TokenStream2 = tokens.iter().cloned().collect();
    syn::parse2::<syn::Expr>(ts).ok()
}
```

This would improve error messages and ensure the emitted code is valid Rust
before the compiler sees it.

#### B. Use `syn::Item` for top-level item validation

After generating a struct/enum/trait/impl, validate the output:

```rust
let output = parse_struct(&tokens)?;
// Optional: validate the generated code parses as a valid Rust item
if let Err(e) = syn::parse2::<syn::Item>(output.clone()) {
    abort!(Span::call_site(), "generated invalid Rust: {}", e);
}
```

#### C. Use `syn::parse::Parse` for the entire S-expression grammar

For maximum `syn` integration, define a custom AST:

```rust
enum LispExpr {
    Lit(syn::Lit),
    Ident(syn::Ident),
    Call { func: Box<LispExpr>, args: Vec<LispExpr> },
    BinOp { op: syn::BinOp, lhs: Box<LispExpr>, rhs: Box<LispExpr> },
    If { cond: Box<LispExpr>, then: Box<LispExpr>, else_: Option<Box<LispExpr>> },
    Let { pat: syn::Pat, ty: Option<syn::Type>, val: Box<LispExpr> },
    Fn(LispFnDef),
    Struct(LispStructDef),
    // ... etc.
}

impl syn::parse::Parse for LispExpr {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        // Parse S-expression grammar using syn's parsing infrastructure
        // This gives us span tracking, error recovery, and lookahead for free
    }
}
```

This is the **most ambitious** approach but would yield:
- **Structured error messages** with precise spans (via `syn::Error`)
- **Composable parsing** (each form is a `Parse` impl)
- **Testability** (parse to AST, then test AST → Rust separately)
- **Extensibility** (add new forms by adding AST variants)

#### D. Use `quote::ToTokens` for code generation

Pair the custom AST with `ToTokens` implementations:

```rust
impl quote::ToTokens for LispExpr {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        match self {
            LispExpr::BinOp { op, lhs, rhs } => {
                lhs.to_tokens(tokens);
                op.to_tokens(tokens);
                rhs.to_tokens(tokens);
            }
            // ...
        }
    }
}
```

This separates parsing from code generation, making both independently
testable and maintainable.

### 7.3 Recommended `syn`/`quote` adoption level

For this project, a **pragmatic middle ground** is recommended:

1. **Keep** the current token-tree-walking approach in `eval_lisp_expr`
   (it works, is well-tested, and handles the DSL's grammar naturally).
2. **Expand** `syn` validation to cover more emitted code (expressions,
   items) as a safety net.
3. **Use** `syn::parse::ParseStream` for new complex forms (e.g., if
   adding `async` blocks, `try` blocks, or pattern matching enhancements).
4. **Defer** the full custom-AST approach until/unless the grammar grows
   significantly larger or error reporting becomes a priority.

---

## 8. Migration Path

### Phase 1: Expose `lisp_eval!` (low risk)

1. Add `pub fn lisp_eval(...)` proc macro that calls `eval_lisp_expr`.
2. Add it to the re-exports in `lisp/src/lib.rs`.
3. **No user-facing changes.** All existing code continues to work.

### Phase 2: Redirect expression arms (medium risk)

1. Replace expression-evaluation arms in `lisp!` with a single catch-all
   that delegates to `lisp_eval!`.
2. Keep item-definition arms (struct, enum, trait, impl, fn) that benefit
   from `$vis:vis` capture.
3. Run the full test suite to verify equivalence.

### Phase 3: Remove dead code (low risk)

1. Remove `lisp_dot_call!`, `lisp_dot_literal!`, `lisp_match_dispatch!`,
   `lisp_match_arg!`.
2. Simplify `lisp_arg!` or internalize it.
3. Update documentation.

### Phase 4: Enhance `syn` integration (optional)

1. Add `syn::Expr` validation for generated expressions.
2. Add `syn::Item` validation for generated items.
3. Improve error messages with `syn::Error` spans.

---

## 9. Benefits of the Proposed Architecture

| Benefit | Description |
|---|---|
| **Single source of truth** | One expression evaluator instead of two |
| **~400 fewer lines** | Eliminating duplicated `macro_rules!` arms |
| **Better error messages** | `syn`-powered parsing gives precise spans |
| **Easier to extend** | Adding a new form means one code change, not two |
| **Better `self` handling** | Proc macro can handle `self` tokens directly without hygiene issues |
| **No callback chains** | No more `::lisp::lisp!(match ...)` from inside proc macros |
| **Testable** | Proc macro logic can be unit-tested with `syn::parse2` |
| **Debug-friendly** | `debug-expansion` feature already exists; a single evaluator makes expansion output clearer |

---

## 10. Risks and Mitigations

| Risk | Mitigation |
|---|---|
| `macro_rules!` is faster at compile time than proc macros | Profile; the difference is likely negligible for typical use |
| Proc macro hygiene differs from `macro_rules!` | The existing proc macros already handle this correctly; test thoroughly |
| `$vis:vis` is not available in proc macros | `syn::Visibility` parsing already works in the codebase |
| `$pat:pat_param` is not available in proc macros | `syn::Pat::parse_multi` already works in the codebase |
| Breaking change for users who depend on internal macros | `lisp!` public API stays identical; internal macros are `#[doc(hidden)]` |

---

## 11. Summary

The current architecture is functional but maintains **two parallel
implementations** of the core S-expression evaluator: one in `macro_rules!`
(~370 lines in `lisp/src/lib.rs`) and one in `eval_lisp_expr` (~600 lines in
`lisp-macro/src/lib.rs`). Every new language feature must be implemented
twice.

The recommended path forward is to:

1. **Unify** on the proc-macro evaluator as the single source of truth.
2. **Reduce** `lisp!` to a thin ~10-arm dispatcher for item definitions
   that benefit from `$vis:vis`.
3. **Deepen** `syn`/`quote` integration incrementally (validation first,
   full custom AST later if needed).
4. **Maintain** backward compatibility — the user-facing `lisp!(...)` API
   does not change.
