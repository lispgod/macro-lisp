# Architecture: `macro_rules!` vs `proc-macro` in macro-lisp

This document analyzes the two macro approaches used in macro-lisp, evaluates
consolidation strategies, and recommends how to make the implementation as clean,
powerful, and closely integrated with `syn` and `quote` as possible.

---

## Table of Contents

1. [Current Architecture Overview](#current-architecture-overview)
2. [What Lives Where Today](#what-lives-where-today)
3. [Analysis: Should We Move Everything to `proc-macro`?](#analysis-should-we-move-everything-to-proc-macro)
4. [Duplication Inventory](#duplication-inventory)
5. [Deduplication Strategy](#deduplication-strategy)
6. [Deeper `syn` and `quote` Integration](#deeper-syn-and-quote-integration)
7. [Recommendations](#recommendations)
8. [Migration Path](#migration-path)

---

## Current Architecture Overview

The project uses a **two-tier architecture**:

```
User code
    │
    ▼
┌─────────────────────────────────────────────────────┐
│  lisp!  (macro_rules! in /lisp/src/lib.rs)          │
│  ─ pattern-matches top-level forms                  │
│  ─ handles literals, closures, modules, imports     │
│  ─ delegates complex forms to proc-macros           │
└─────────┬──────────────┬───────────────┬────────────┘
          │              │               │
          ▼              ▼               ▼
   ┌────────────┐ ┌────────────┐ ┌────────────────┐
   │ lisp_fn!   │ │lisp_struct!│ │  lisp_eval!    │
   │ lisp_impl! │ │ lisp_enum! │ │  lisp_let!     │
   │ lisp_trait!│ │            │ │  lisp_assign!  │
   └────────────┘ └────────────┘ └────────────────┘
          └──────────────┴───────────────┘
                         │
              /lisp-macro/src/lib.rs
              (proc-macro crate using syn + quote)
```

### Crate dependency chain

```
macro_lisp (root)  ──depends on──▶  lisp  ──depends on──▶  lisp-macro
    re-exports lisp::lisp!           re-exports              proc_macro = true
                                     lisp_macro::*            syn, quote,
                                     defines lisp! +          proc-macro-error2
                                     lisp_arg!
```

---

## What Lives Where Today

### `/lisp/src/lib.rs` — `macro_rules!` (≈130 lines of macro arms)

| Form | What `lisp!` does | Could proc-macro handle it? |
|---|---|---|
| `(false)`, `(true)` | Emits literal directly | Yes, `lisp_eval!` already handles these |
| `(self.x.y)` | Emits the path directly | Yes, `lisp_eval!` already handles `self` |
| `(struct ...)` | Dispatches to `lisp_struct!` | Already delegated |
| `(enum ... { })` | Passthrough with brace body | Yes — but this is a raw Rust passthrough |
| `(enum ... (...)+)` | Dispatches to `lisp_enum!` | Already delegated |
| `(trait ...)` | Dispatches to `lisp_trait!` | Already delegated |
| `(impl ...)` | Dispatches to `lisp_impl!` | Already delegated |
| `(type Name = T)` | Emits type alias | Yes |
| `(const fn ...)` | Dispatches to `lisp_fn!` | Already delegated |
| `(const N T = v)` | Emits const item | Yes |
| `(static ...)` | Emits static item | Yes |
| `(extern crate n)` | Emits extern crate | Yes |
| `(use path)` | Emits use statement | Yes |
| `(mod name (...))` | Emits module, recursively calls `lisp!` | **Harder** — see below |
| Closures (12 arms) | Emits closures directly | Yes, `lisp_eval!` already handles `fn` closures |
| Named fns | Dispatches to `lisp_fn!` | Already delegated |
| `(rust {...})` | Escape hatch | Yes |
| Catch-all `($($t)+)` | Dispatches to `lisp_eval!` | Already delegated |

### `/lisp/src/lib.rs` — `lisp_arg!` (3 lines)

```rust
macro_rules! lisp_arg {
    ( ( $($e:tt)* ) ) => ( $crate::lisp!( $($e)* ) );
    ($e:expr) => ($e);
}
```

This is the recursive glue: parenthesized groups call back into `lisp!`, bare
expressions pass through.

### `/lisp-macro/src/lib.rs` — proc-macros (≈2400 lines)

8 exported proc-macros: `lisp_eval`, `lisp_fn`, `lisp_let`, `lisp_struct`,
`lisp_enum`, `lisp_trait`, `lisp_impl`, `lisp_assign`.

The core engine is `eval_lisp_expr()` (~530 lines), which handles all
expression-level S-expressions: operators, control flow, bindings, collections,
references, function calls, method calls, and macro invocations.

---

## Analysis: Should We Move Everything to `proc-macro`?

### What `macro_rules!` does well here

1. **Zero-cost dispatch**: Pattern-matching on the first keyword (`struct`,
   `enum`, `fn`, `let`, etc.) is instantaneous. The compiler resolves the arm at
   expansion time with no procedural overhead.

2. **Recursive `mod` handling**: The `mod` form recursively calls `lisp!` for
   each top-level item inside the module. A proc-macro cannot invoke another
   macro during expansion — it can only emit tokens that *contain* macro calls.
   The current `macro_rules!` approach naturally handles this:
   ```rust
   $vis mod $sym { $( $crate::lisp!( $($e)* ); )* }
   ```
   A proc-macro would have to emit `lisp!(...)` calls in the output, which is
   functionally equivalent but adds an extra expansion step.

3. **`use` / `extern crate`**: These are trivial pattern matches that add a
   semicolon. Moving them to a proc-macro would add complexity for no benefit.

4. **Enum brace passthrough**: `(enum Name { variants })` passes raw Rust
   through. This is a convenience form that doesn't need parsing.

### What `macro_rules!` does poorly here

1. **Closures are duplicated**: The 12 closure arms in `lisp!` duplicate logic
   that `eval_closure()` in the proc-macro already handles — with better support
   for return types, parameter validation via `syn::Type`, and arbitrary nesting.

2. **`const` / `static` items lack `syn` validation**: The `macro_rules!` arms
   for `(const N T v)` and `(static N T v)` use `$typ:ty` and `$val:expr`
   fragment specifiers. These work but cannot validate the type against
   `syn::Type` or evaluate the value through `eval_lisp_expr`.

3. **`type` alias lacks flexibility**: The arm `($vis:vis type $name:ident = $target:ty)`
   only accepts `syn`-parseable types via `$target:ty`. A proc-macro could
   accept arbitrary tokens and validate with `syn::parse2::<syn::Type>()`.

4. **Literals `(true)` / `(false)` are redundant**: `eval_lisp_expr` already
   handles these identically.

5. **`(self.x.y)` is redundant**: `eval_lisp_expr` already handles the `self`
   keyword identically.

### Verdict

**Move closures, `const`/`static`/`type` items, and redundant literal arms to
the proc-macro. Keep `mod`, `use`, `extern crate`, and the enum brace
passthrough in `macro_rules!`.** The dispatcher role of `lisp!` should remain in
`macro_rules!`, but it should be thinner — delegating everything that benefits
from `syn` validation to the proc-macro.

---

## Duplication Inventory

| Feature | `macro_rules!` location | `proc-macro` location | Duplicated? |
|---|---|---|---|
| `true` / `false` | `lisp!` arms (lines 214–215) | `eval_lisp_expr` `"true"` / `"false"` | **Yes** |
| `self.x.y` | `lisp!` arm (line 216) | `eval_lisp_expr` `"self"` branch | **Yes** |
| Closures | 12 `lisp!` arms (lines 272–313) | `eval_closure()` (~70 lines) | **Yes — significant** |
| `const` / `static` | 6 `lisp!` arms (lines 242–249) | Not in proc-macro | No (but could benefit from `syn`) |
| `type` alias | 1 `lisp!` arm (line 236) | Not in proc-macro | No (but could benefit from `syn`) |
| `rust` escape | 2 `lisp!` arms (lines 325–326) | `eval_lisp_expr` `"rust"` branch | **Yes** |
| `lisp_arg!` | `macro_rules!` (3 lines) | `eval_lisp_arg()` function | **Parallel** — not exactly duplicated since they operate at different levels |

### Closure duplication detail

The `macro_rules!` closure arms handle:
- Zero-param: `(fn () body)`, `(fn move () body)`
- Zero-param with return type: `(fn () -> Ret body)`, `(fn move () -> Ret body)`
- Typed params: `(fn ((x T)...) body)`, `(fn move ((x T)...) body)`
- Typed params with return type: `(fn ((x T)...) -> Ret body)`, `(fn move ((x T)...) -> Ret body)`
- Untyped params: `(fn ((x)...) body)`, `(fn move ((x)...) body)`
- Untyped params with return type: `(fn ((x)...) -> Ret body)`, `(fn move ((x)...) -> Ret body)`

Meanwhile, `eval_closure()` in the proc-macro handles **all** of these in one
unified function with:
- Move detection via `is_ident(&tokens[i], "move")`
- Parameter parsing via `validate_type()` (calling `syn::parse2::<syn::Type>()`)
- Return type parsing with `syn::Type` validation
- Body evaluation via `parse_body_items()` → `eval_lisp_expr()`

The proc-macro version is strictly more capable and already covers every case the
`macro_rules!` arms handle.

---

## Deduplication Strategy

### Step 1: Remove the 12 closure arms from `lisp!`

The catch-all `($($t:tt)+) => ($crate::lisp_eval!($($t)+))` at the bottom of
`lisp!` already forwards unmatched forms to `lisp_eval!`. Since closures start
with `fn (` (not `fn $name`), they would naturally fall through to `lisp_eval!`
→ `eval_lisp_expr` → `eval_closure()` if the `macro_rules!` arms were removed.

**Caveat**: The current `macro_rules!` closure arms recursively call `lisp!` for
body expressions. The proc-macro `eval_closure()` calls `eval_lisp_expr`
instead. This is functionally equivalent for expressions but differs for
top-level items (e.g., closures that define structs inside them). In practice,
closures don't define items, so this is safe.

**Action**: Remove closure arms (lines 272–313), verify all closure tests pass.

### Step 2: Remove redundant literal arms

Remove `(false) => (false)` and `(true) => (true)` from `lisp!`. These are
already handled by `eval_lisp_expr`.

Remove `(self $(. $e:tt)*)` — already handled by `eval_lisp_expr`'s `"self"`
branch.

**Action**: Remove lines 213–216, verify tests pass.

### Step 3: Remove redundant `(rust ...)` arms

`eval_lisp_expr` handles `"rust"` in its keyword dispatch. The `macro_rules!`
arms for `(rust { ... })` and `(rust stmt...)` can be removed.

**Action**: Remove lines 325–326, verify tests pass.

### Step 4: Move `const`/`static`/`type` to a proc-macro handler

Add `const`, `static`, and `type` handling to `lisp_eval!` (or create a new
`lisp_item!` proc-macro). This enables `syn::Type` validation for the type
annotation and `eval_lisp_expr` for the value.

**Action**: Add item-level handling to the proc-macro, remove corresponding
`macro_rules!` arms.

### Step 5: Unify `lisp_arg!` with `eval_lisp_arg()`

`lisp_arg!` is a `macro_rules!` macro that:
1. Unwraps parenthesized groups → calls `lisp!`
2. Falls through for bare expressions

`eval_lisp_arg()` is a proc-macro function that:
1. Unwraps parenthesized groups → calls `eval_lisp_expr`
2. Falls through for bare tokens

These serve the same purpose at different levels. After the above consolidation,
`lisp_arg!` is only needed in the `macro_rules!` arms that survive (primarily
`const`/`static` without `=`). If those move to the proc-macro, `lisp_arg!` can
be removed entirely.

### Resulting `lisp!` after deduplication

```rust
macro_rules! lisp {
    // Item definitions — dispatch to proc-macros
    ( $(#[$m:meta])* $vis:vis struct $name:ident $($rest:tt)* ) => {
        $crate::lisp_struct!($(#[$m])* $vis struct $name $($rest)*);
    };
    ( $(#[$m:meta])* $vis:vis enum $name:ident { $($body:tt)* }) => {
        $(#[$m]);* $vis enum $name { $($body)* }
    };
    ( $(#[$m:meta])* $vis:vis enum $name:ident $($rest:tt)+ ) => {
        $crate::lisp_enum!($(#[$m])* $vis enum $name $($rest)+);
    };
    ( $(#[$m:meta])* $vis:vis trait $name:ident $($rest:tt)* ) => {
        $crate::lisp_trait!($vis $name $($rest)*);
    };
    (impl $($tokens:tt)+) => {
        $crate::lisp_impl!($($tokens)+);
    };

    // Named functions — all dispatch to lisp_fn! proc-macro
    ( $(#[$m:meta])* $vis:vis unsafe fn $sym:ident $($rest:tt)+ ) => {
        $crate::lisp_fn!($(#[$m])* $vis unsafe fn $sym $($rest)+);
    };
    ( $(#[$m:meta])* $vis:vis async fn $sym:ident $($rest:tt)+ ) => {
        $crate::lisp_fn!($(#[$m])* $vis async fn $sym $($rest)+);
    };
    ( $(#[$m:meta])* $vis:vis extern $abi:literal fn $sym:ident $($rest:tt)+ ) => {
        $crate::lisp_fn!($(#[$m])* $vis extern $abi fn $sym $($rest)+);
    };
    ( $(#[$m:meta])* $vis:vis const fn $sym:ident $($rest:tt)+ ) => {
        $crate::lisp_fn!($(#[$m])* $vis const fn $sym $($rest)+);
    };
    ( $(#[$m:meta])* $vis:vis fn $sym:ident $($rest:tt)+ ) => {
        $crate::lisp_fn!($(#[$m])* $vis fn $sym $($rest)+);
    };

    // Imports & modules — keep in macro_rules! (recursive lisp! for mod)
    ( $(#[$m:meta])* extern crate $sym:ident) => {
        $(#[$m]);* extern crate $sym;
    };
    (use $sym:tt $(:: $sym2:tt)* ) => {
        use $sym $(:: $sym2)* ;
    };
    ( $(#[$m:meta])* $vis:vis mod $sym:ident $( ( $($e:tt)* ))* ) => {
        $(#[$m]);* $vis mod $sym { $( $crate::lisp!( $($e)* ); )* }
    };

    // Everything else → proc-macro
    ($($t:tt)+) => ($crate::lisp_eval!($($t)+));
}
```

This reduces the `macro_rules!` surface from ~120 lines of arms to ~40 lines,
eliminating all duplication while keeping the forms that genuinely benefit from
`macro_rules!` (modules with recursive expansion, `use`, `extern crate`).

---

## Deeper `syn` and `quote` Integration

The proc-macro crate currently uses `syn` and `quote` but operates primarily on
raw `TokenTree` slices. This works but misses opportunities for stronger
validation and cleaner code generation.

### Current `syn` usage

| Function | `syn` feature used |
|---|---|
| `validate_type()` | `syn::parse2::<syn::Type>()` — validates then re-quotes |
| `validate_pattern()` | `syn::parse2::<syn::Pat>()` via `Pat::parse_multi` |
| `validate_generics()` | `syn::parse2::<syn::Generics>()` — validates angle brackets |
| `parse_type_list()` | Custom `syn::parse::Parse` impl for greedy type list |
| `parse_visibility()` | `syn::parse2::<syn::Visibility>()` — validates pub forms |
| `parse_fn_signature()` | Uses the above helpers, but parameter parsing is manual |

### Opportunities for deeper integration

#### 1. Use `syn::parse::Parse` for the entire S-expression grammar

Instead of manual `TokenTree` indexing, define custom `syn` parse types:

```rust
struct LispExpr { /* ... */ }

impl syn::parse::Parse for LispExpr {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        // Use input.peek(), input.parse::<Ident>()?, etc.
        // This gives proper span tracking and error messages for free.
    }
}
```

**Benefits**:
- `input.span()` gives precise error locations
- `input.error("message")` produces compiler-quality diagnostics
- `input.peek(Token![let])` for lookahead without consuming
- `input.parse::<syn::Type>()?` for inline type parsing
- `Punctuated::parse_terminated` for comma-separated lists

**Example — parsing `(let (var Type) val)` with `syn`**:

```rust
struct LetBinding {
    is_mut: bool,
    name: Ident,
    ty: Option<syn::Type>,
    value: LispExpr,
}

impl Parse for LetBinding {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let is_mut = input.peek(Token![mut]);
        if is_mut { input.parse::<Token![mut]>()?; }

        let (name, ty) = if input.peek(syn::token::Paren) {
            let content;
            syn::parenthesized!(content in input);
            let name = content.parse::<Ident>()?;
            let ty = content.parse::<syn::Type>()?;
            (name, Some(ty))
        } else {
            (input.parse::<Ident>()?, None)
        };

        let value = input.parse::<LispExpr>()?;
        Ok(LetBinding { is_mut, name, ty, value })
    }
}
```

#### 2. Use `quote!` with `syn` types directly

Currently the code does:
```rust
let raw_type: TokenStream2 = inner[1..].iter().cloned().collect();
let param_type = validate_type(raw_type);
params.push(quote! { #param_name: #param_type });
```

With `syn` types:
```rust
let param_type: syn::Type = syn::parse2(inner[1..].iter().cloned().collect())?;
params.push(quote! { #param_name: #param_type });
```

`syn::Type` implements `quote::ToTokens`, so it can be interpolated directly
into `quote!` with no intermediate `TokenStream2`. This applies to all `syn`
types: `syn::Pat`, `syn::Generics`, `syn::Visibility`, `syn::Expr`, etc.

#### 3. Replace manual token-tree walking with `syn::parse::ParseBuffer`

The current pattern:
```rust
fn parse_foo(tokens: &[TokenTree]) -> TokenStream2 {
    let mut i = 0;
    // manual index tracking, bounds checking, type casting...
    if i < tokens.len() && is_ident(&tokens[i], "pub") { ... }
}
```

The `syn` equivalent:
```rust
fn parse_foo(input: ParseStream) -> syn::Result<TokenStream2> {
    let vis: syn::Visibility = input.parse()?;
    let name: Ident = input.parse()?;
    // ...
}
```

This eliminates entire categories of bugs (off-by-one, forgotten bounds checks,
incorrect span propagation) and produces better error messages.

#### 4. Use `syn::Item` variants for item-level forms

For `struct`, `enum`, `trait`, and `impl`, the proc-macro could build `syn::Item`
variants directly:

```rust
let item = syn::ItemStruct {
    attrs: parsed_attrs,
    vis: parsed_vis,
    struct_token: Default::default(),
    ident: name,
    generics: parsed_generics,
    fields: syn::Fields::Named(named_fields),
    semi_token: None,
};
quote! { #item }
```

This guarantees the output is syntactically valid Rust, since `syn::ItemStruct`
can only represent valid struct definitions.

#### 5. Use `syn::Expr` for expression validation

After building a Rust expression via `quote!`, parse it back through
`syn::parse2::<syn::Expr>()` as a validation step. This catches malformed
expansions at macro-expansion time rather than letting them become cryptic
compiler errors.

---

## Recommendations

### Short-term (minimal changes, high impact)

1. **Remove the 12 closure arms** from `lisp!`. The catch-all already routes to
   `lisp_eval!` → `eval_closure()`, which is strictly more capable.

2. **Remove redundant `true`/`false`/`self`/`rust` arms**. These are already
   handled identically by `eval_lisp_expr`.

3. **Move `const`/`static`/`type` to the proc-macro** for `syn::Type`
   validation. Add keyword handling in `eval_lisp_expr` or create item-level
   dispatch in a new `lisp_item!` proc-macro.

### Medium-term (moderate refactor, significant cleanup)

4. **Refactor proc-macro internals to use `syn::parse::Parse`** instead of
   manual `&[TokenTree]` indexing. Start with `parse_fn_signature` and
   `eval_let`, which are the most complex parsers.

5. **Build `syn` AST types** (`syn::ItemFn`, `syn::ItemStruct`, etc.) instead of
   assembling `quote!` fragments. This gives structural validation for free.

6. **Remove `lisp_arg!`** once all `macro_rules!` arms that use it are migrated.

### Long-term (architectural evolution)

7. **Define a `LispForm` enum** implementing `syn::parse::Parse` that represents
   the full S-expression grammar. This becomes the single entry point for all
   parsing:
   ```rust
   enum LispForm {
       Literal(syn::Lit),
       Let(LetBinding),
       If(IfExpr),
       Match(MatchExpr),
       FnDef(FnDef),
       FnCall(FnCall),
       BinaryOp(BinaryOp),
       // ...
   }
   ```

8. **Reduce to two macros total**: `lisp!` (the `macro_rules!` dispatcher for
   top-level items + `mod` recursion) and `lisp_eval!` (the single proc-macro
   for everything else). The intermediate proc-macros (`lisp_fn!`, `lisp_struct!`,
   etc.) would become internal functions called by `lisp_eval!` based on the
   first keyword.

9. **Unify error reporting** through `syn::Error` and `proc_macro_error2::abort!`
   with span-accurate diagnostics for every form.

---

## Migration Path

### Phase 1: Deduplicate (no behavior changes)

```
1. Delete closure arms from lisp! (12 arms)
2. Delete true/false/self/rust arms from lisp! (5 arms)
3. Verify: cargo test (all existing tests must pass)
```

### Phase 2: Consolidate item forms

```
4. Add const/static/type handling to eval_lisp_expr or lisp_eval!
5. Delete const/static/type arms from lisp!
6. Delete lisp_arg! (no longer referenced)
7. Verify: cargo test
```

### Phase 3: Strengthen with `syn`

```
8. Refactor parse_fn_signature to use syn::parse::Parse
9. Refactor eval_let to use syn::parse::Parse
10. Refactor parse_struct / parse_enum to build syn::Item types
11. Add syn::Expr validation as post-expansion check
12. Verify: cargo test + new tests for error messages
```

### Phase 4: Simplify public API

```
13. Merge lisp_fn!, lisp_struct!, lisp_enum!, lisp_trait! into lisp_eval!
14. Keep only lisp! (macro_rules!) + lisp_eval! (proc-macro) as public API
15. Keep lisp_impl! separate only if needed for disambiguation
16. Verify: cargo test + backward compatibility
```

---

## Summary

| Metric | Current | After deduplication | After full `syn` integration |
|---|---|---|---|
| `macro_rules!` arms | ~30 | ~15 | ~15 |
| Proc-macro exports | 8 | 8 (then reduce to 2) | 2 |
| Closure handling locations | 2 | 1 | 1 |
| `syn::Type` validation | Partial | More | Full |
| Error message quality | Mixed | Improved | Compiler-grade |
| Lines in `lisp!` | ~130 | ~60 | ~60 |
| Lines in proc-macro | ~2400 | ~2500 (absorbs items) | ~2000 (cleaner with `syn::Parse`) |

The key insight is that **`macro_rules!` should be a thin dispatcher, not an
evaluator**. Forms that benefit from `syn` validation (types, patterns, generics,
expressions) belong in the proc-macro. Forms that benefit from recursive macro
expansion (`mod`) or are trivial boilerplate (`use`, `extern crate`) stay in
`macro_rules!`.
