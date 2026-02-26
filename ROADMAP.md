# macro-lisp Roadmap

Current state of the implementation based on source analysis. Organized by priority.

---

## Broken

Nothing is critically broken — all existing tests pass. The macro dispatches correctly for every documented form.

---

## Missing

### Zero-parameter closures
**What:** `(fn () expr)` is intended to create a closure `|| expr`, but the macro_rules pattern matches the named-function rule instead (the `fn` keyword followed by empty parens looks like a void function declaration to the parser).
**Fix:** Add a dedicated closure rule for `(fn () ...)` above the named-function rules, or use a different syntax like `(fn (()) ...)`.
**Complexity:** Moderate — requires careful rule ordering in `macro_rules!` to avoid ambiguity.

### `array-repeat` construction
**What:** Rust's `[value; count]` repeat syntax has no lisp form. You can use `(rust { [0; 100] })` as a workaround.
**Fix:** Add `(array-repeat val count)` → `[val; count]` rule.
**Complexity:** Trivial.

### Tuple struct construction via `new`
**What:** `(new Name val1 val2)` for tuple structs doesn't work — `new` always generates `Name { field: val }` syntax. Workaround: use `(Name val1 val2)` which hits the function-call catch-all.
**Fix:** Detect when arguments don't have `(field val)` pairs and emit `Name(val1, val2)` instead.
**Complexity:** Moderate.

### `if let` chains / `else if`
**What:** There's no way to write `if ... else if ...` chains directly. The two-branch `(if cond then else)` forces nesting.
**Fix:** Consider `(cond (test1 body1) (test2 body2) (else body3))` or similar.
**Complexity:** Moderate.

### Enum variant construction helpers
**What:** Constructing enum struct variants requires `rust { EnumName::Variant { field: val } }`. The `new` form only works for top-level struct names, not path-qualified enum variants.
**Fix:** Allow `(new Path::Variant (field val) ...)`.
**Complexity:** Moderate — `new` currently only accepts a single ident.

---

## Incomplete

### Associated type defaults in traits
**What:** `(type Output = i32)` inside a trait definition emits `type Output = i32;`, which requires the `associated_type_defaults` nightly feature. It works in impl blocks.
**Fix:** In trait context, emit `type Output;` (declaration only) by default. Associated type defaults should only be emitted in nightly-gated builds.
**Complexity:** Moderate — need to distinguish trait vs impl context in `parse_impl_body_item`.

### Complex types in `let` bindings
**What:** `(let (var Type) val)` works for simple types but can fail with complex type expressions that don't fit in a single `$typ:ty` fragment (e.g., types containing `>` that confuse the macro parser).
**Fix:** Already partially handled by the proc macro fallback (`lisp_let!`), but edge cases with deeply nested generic types may still fail.
**Complexity:** Moderate.

### Closures with return type annotation
**What:** No way to annotate a closure's return type: `(fn ((x i32)) -> i32 (+ x 1))`. The return type position is only parsed for named functions.
**Fix:** Add a closure form that accepts a return type between the params group and the body.
**Complexity:** Moderate.

### Multiple match arm bodies
**What:** Match arms only accept a single body expression. `(match x (1 => (stmt1) (stmt2)))` doesn't chain multiple statements. Workaround: wrap in `(block ...)`.
**Fix:** Allow `(pattern => (body1) (body2) ...)` to emit `pattern => { body1; body2 }`.
**Complexity:** Moderate — requires changes to the match macro rule.

### `for` with pattern destructuring
**What:** `(for (a, b) in iter (body))` doesn't work because the `for` rule expects `$var:ident`. Workaround: use `(rust { for (a, b) in iter { ... } })`.
**Fix:** Accept a pattern instead of a single ident in the `for` rule.
**Complexity:** Moderate — `$var:pat` would need to be balanced against the `in` keyword.

### Method calls in proc macro body expressions
**What:** Inside proc macro bodies (impl/fn), method calls like `(self.field.method args)` work, but chained method calls like `(result.unwrap.to_string)` may not parse correctly due to how `eval_lisp_expr` handles the `.` operator.
**Fix:** Improve the method-call detection in `eval_lisp_expr` to handle longer chains.
**Complexity:** Moderate.

### Struct update syntax in proc macro bodies
**What:** The `(new Name (f1 v1) (.. base))` spread syntax works in macro_rules but may not work inside proc macro body expressions (impl methods) since `eval_lisp_expr`'s `new` handler doesn't handle `..`.
**Fix:** Add `..` handling to `eval_lisp_expr`'s `new` case.
**Complexity:** Trivial.

---

## Quality

### Error messages for mismatched forms
**What:** When a form doesn't match any macro rule, the error message is the default macro_rules "no rules expected the token" error, which is confusing.
**Fix:** Add a catch-all error rule with `compile_error!` at the end of common forms.
**Complexity:** Trivial.

### Span accuracy in proc macros
**What:** Proc macro errors sometimes point to the macro invocation site rather than the specific offending token. The proc macros use `proc_macro_error2` for `abort!` but some paths construct errors with `Span::call_site()`.
**Fix:** Thread source spans through more carefully in `eval_lisp_expr` and related functions.
**Complexity:** Moderate.

### Test coverage: `async`/`await`
**What:** Async fn definitions compile but there's no runtime test of async execution (would need tokio or similar). The syntax is validated at compile time only.
**Fix:** Add an optional `tokio` dev-dependency for async tests, or use `futures::executor::block_on`.
**Complexity:** Trivial.

### Test coverage: `extern crate`
**What:** `extern crate` is documented but has no dedicated test (it's a legacy form mainly for 2015 edition code).
**Fix:** Add a simple test that uses `extern crate`.
**Complexity:** Trivial.

### Test coverage: `pub(super)` and `pub(in path)`
**What:** Only `pub` and `pub(crate)` are tested. `pub(super)` and `pub(in path)` are accepted by the parser but not exercised.
**Fix:** Add tests inside nested modules.
**Complexity:** Trivial.

### Doc tests
**What:** No doc tests run (`Doc-tests macro_lisp: 0 tests`). The README examples aren't tested.
**Fix:** Add `#[doc = include_str!("../README.md")]` or proper `///` doc examples in `src/lib.rs`.
**Complexity:** Trivial.

---

## Future

### Pattern matching in `for` loops
**What:** Allow `(for (k, v) in map (body))` with pattern destructuring.
**Complexity:** Moderate.

### `match` with guard clauses
**What:** `(match x (n if (> n 0) => body))` — pattern guards.
**Complexity:** Moderate — macro_rules `$:pat_param` doesn't capture guards; needs proc macro.

### Variadic bitwise operators
**What:** `(& a b c)` should expand to `a & b & c`, like arithmetic operators do. Currently bitwise ops are binary only.
**Complexity:** Trivial.

### `impl` with lifetime parameters
**What:** `(impl<'a> Trait for Type<'a> ...)` works but could be better documented and tested.
**Complexity:** Trivial (testing only).

### Tuple indexing
**What:** `(. tuple 0)` doesn't work because `0` is a literal, not an ident. Workaround: `(rust { tuple.0 })`.
**Fix:** Handle numeric literals after `.` in the field-access rule.
**Complexity:** Moderate.

### `let else`
**What:** Rust's `let Some(x) = opt else { return; }` pattern has no lisp form.
**Fix:** `(let else (Pat = expr) (fallback))`.
**Complexity:** Moderate.

### Slice patterns
**What:** `(let [first, rest @ ..] = arr)` — complex slice destructuring. Works via `lisp_let!` fallback but no dedicated syntax.
**Complexity:** Trivial (already works through fallback).

### `dyn` and `impl Trait` in argument position
**What:** `(fn foo ((x &dyn Trait)) ...)` works because `&dyn Trait` is a valid `$typ:ty`. But explicit `(fn foo ((x impl Trait)) ...)` for `impl Trait` argument position may need testing.
**Complexity:** Trivial (testing only).

### Multiline string / raw string support
**What:** Raw strings `r#"..."#` and multiline strings work as Rust literals but aren't documented.
**Complexity:** Trivial (documentation only).

### `loop` label expressions
**What:** `('label loop (body))` produces a labeled loop that can be `break`'d with a value, enabling `let x = 'label: loop { break 'label val; }`. Already works.
**Complexity:** N/A (already implemented).

### IDE support / syntax highlighting
**What:** No editor plugin for highlighting lisp! macro contents. VS Code, IntelliJ users see raw token streams.
**Fix:** Create a TextMate grammar or tree-sitter parser for the DSL.
**Complexity:** Hard.
