//! `lisp!` — Write Rust code in S-expression (Lisp-like) syntax.
//!
//! # Visibility
//!
//! All item definitions (struct, enum, trait, fn, const, static, type, mod)
//! accept any Rust visibility modifier uniformly via `$vis:vis`:
//! `pub`, `pub(crate)`, `pub(super)`, `pub(in path)`, or no modifier.
//!
//! # Supported Forms
//!
//! ## Literals
//! | S-expression | Rust output |
//! |---|---|
//! | `(false)` | `false` |
//! | `(true)` | `true` |
//! | `(self.x.y)` | `self.x.y` |
//!
//! ## Type & Item Definitions
//! | S-expression | Rust output |
//! |---|---|
//! | `(struct Name ((field Type)...))` | `struct Name { field: Type, ... }` |
//! | `(pub struct Name ((field Type)...))` | `pub struct Name { field: Type, ... }` |
//! | `(pub(crate) struct Name ...)` | `pub(crate) struct Name { ... }` |
//! | `(struct Name (Type1 Type2))` | `struct Name(Type1, Type2);` (tuple struct) |
//! | `(enum Name { variants })` | `enum Name { variants }` |
//! | `(pub enum Name { variants })` | `pub enum Name { variants }` |
//! | `(trait Name (fn-decl)...)` | `trait Name { fn-decl... }` |
//! | `(pub trait Name (fn-decl)...)` | `pub trait Name { fn-decl... }` |
//! | `(impl Trait for Type (method)...)` | `impl Trait for Type { method... }` |
//! | `(impl Type (method)...)` | `impl Type { method... }` |
//! | `(type Name = Target)` | `type Name = Target;` |
//! | `(pub type Name = Target)` | `pub type Name = Target;` |
//! | `(const NAME Type = val)` | `const NAME: Type = val;` |
//! | `(const NAME Type val)` | `const NAME: Type = val;` |
//! | `(pub const NAME Type = val)` | `pub const NAME: Type = val;` |
//! | `(static NAME Type = val)` | `static NAME: Type = val;` |
//! | `(static NAME Type val)` | `static NAME: Type = val;` |
//! | `(static mut NAME Type = val)` | `static mut NAME: Type = val;` |
//! | `(static mut NAME Type val)` | `static mut NAME: Type = val;` |
//!
//! ## Pattern Matching
//! | S-expression | Rust output |
//! |---|---|
//! | `(match e (pat => (body))...)` | `match e { pat => body, ... }` |
//!
//! ## Bindings & Assignment
//! | S-expression | Rust output |
//! |---|---|
//! | `(let mut (var Type) (expr))` | `let mut var: Type = expr;` |
//! | `(let mut var (expr))` | `let mut var = expr;` |
//! | `(let mut var val)` | `let mut var = val;` |
//! | `(let (var Type) (expr))` | `let var: Type = expr;` |
//! | `(let var (expr))` | `let var = expr;` |
//! | `(let var val)` | `let var = val;` |
//! | `(let ((x init)...) (body)...)` | `{ let mut x = init; ... body }` |
//! | `(= var (expr))` | `var = expr;` |
//! | `(= var val)` | `var = val;` |
//! | `(+= var e)` | `var += e;` |
//! | `(-= var e)` | `var -= e;` |
//! | `(*= var e)` | `var *= e;` |
//! | `(/= var e)` | `var /= e;` |
//! | `(%= var e)` | `var %= e;` |
//! | `(&= var e)` | `var &= e;` |
//! | `(\|= var e)` | `var \|= e;` |
//! | `(^= var e)` | `var ^= e;` |
//! | `(<<= var e)` | `var <<= e;` |
//! | `(>>= var e)` | `var >>= e;` |
//!
//! ## Block
//! | S-expression | Rust output |
//! |---|---|
//! | `(block (stmt)...)` | `{ stmt; ... }` |
//! | `(block 'label (stmt)...)` | `'label: { stmt; ... }` |
//!
//! ## Loops
//! | S-expression | Rust output |
//! |---|---|
//! | `(break)` | `break;` |
//! | `(break expr)` | `break expr;` |
//! | `(break 'label)` | `break 'label;` |
//! | `(break 'label expr)` | `break 'label expr;` |
//! | `(continue)` | `continue;` |
//! | `(continue 'label)` | `continue 'label;` |
//! | `(loop (body)...)` | `loop { body }` |
//! | `('label loop (body)...)` | `'label: loop { body }` |
//! | `(while let (Pat = e) (body)...)` | `while let Pat = e { body }` |
//! | `(while cond (body)...)` | `while cond { body }` |
//! | `('label while cond (body)...)` | `'label: while cond { body }` |
//! | `(for var in iter (body)...)` | `for var in iter { body }` |
//! | `('label for var in iter (body)...)` | `'label: for var in iter { body }` |
//!
//! ## Conditionals
//! | S-expression | Rust output |
//! |---|---|
//! | `(if let (Pat = e) then else)` | `if let Pat = e { then } else { else }` |
//! | `(if let (Pat = e) then)` | `if let Pat = e { then }` |
//! | `(if (cond) then else)` | `if cond { then } else { else }` |
//! | `(if cond then else)` | `if cond { then } else { else }` |
//!
//! ## Imports & Modules
//! | S-expression | Rust output |
//! |---|---|
//! | `(extern crate name)` | `extern crate name;` |
//! | `(use path::to::item)` | `use path::to::item;` |
//! | `(pub mod name (body)...)` | `pub mod name { body }` |
//! | `(mod name (body)...)` | `mod name { body }` |

//!
//! ## Functions & Closures
//! | S-expression | Rust output |
//! |---|---|
//! | `(fn move ((x Type)...) (body)...)` | `move \|x: Type\| { body }` |
//! | `(fn ((x Type)...) (body)...)` | `\|x: Type\| { body }` |
//! | `(async fn name ((x T)) Ret (body))` | `async fn name(x: T) -> Ret { body }` |
//! | `(pub fn name ((x T)) Ret (body))` | `pub fn name(x: T) -> Ret { body }` |
//! | `(fn name ((x T)) Ret (body))` | `fn name(x: T) -> Ret { body }` |
//! | `(const fn name ((x T)) Ret (body))` | `const fn name(x: T) -> Ret { body }` |
//! | `(unsafe fn name ((x T)) Ret (body))` | `unsafe fn name(x: T) -> Ret { body }` |
//! | `(extern "C" fn name ((x T)) Ret (body))` | `extern "C" fn name(x: T) -> Ret { body }` |
//! | `(pub const fn name ((x T)) Ret (body))` | `pub const fn name(x: T) -> Ret { body }` |
//! | `(pub unsafe fn name ((x T)) Ret (body))` | `pub unsafe fn name(x: T) -> Ret { body }` |
//! | `(pub extern "C" fn name ((x T)) Ret (body))` | `pub extern "C" fn name(x: T) -> Ret { body }` |
//!
//! ## Control Flow
//! | S-expression | Rust output |
//! |---|---|
//! | `(return (expr))` | `return expr` |
//! | `(return val)` | `return val` |
//! | `(return)` | `return` |
//! | `(unsafe (body)...)` | `unsafe { body }` |
//! | `(await e)` | `e.await` |
//!
//! ## Comparison
//! | S-expression | Rust output |
//! |---|---|
//! | `(== x y)` | `x == y` |
//! | `(!= x y)` | `x != y` |
//! | `(< x y)` | `x < y` |
//! | `(> x y)` | `x > y` |
//! | `(<= x y)` | `x <= y` |
//! | `(>= x y)` | `x >= y` |
//!
//! ## Assertions & Output
//! | S-expression | Rust output |
//! |---|---|
//! | `(panic args...)` | `panic!(args...)` |
//! | `(name! args...)` | `name!(args...)` |
//! | `(macro! name args...)` | `name!(args...)` |
//!
//! ## Logical Operators
//! | S-expression | Rust output |
//! |---|---|

//! | `(&& a b)` | `a && b` |
//! | `(\|\| a b)` | `a \|\| b` |
//! | `(! x)` | `!x` |
//!
//! ## Arithmetic
//! | S-expression | Rust output |
//! |---|---|
//! | `(+ a b ...)` | `a + b + ...` |
//! | `(- a b ...)` | `a - b - ...` |
//! | `(* a b ...)` | `a * b * ...` |
//! | `(/ a b ...)` | `a / b / ...` |
//! | `(% a b)` | `a % b` |
//! | `(neg x)` | `-x` |
//!
//! ## Bitwise Operators
//! | S-expression | Rust output |
//! |---|---|
//! | `(& a b)` | `a & b` |
//! | `(\| a b)` | `a \| b` |
//! | `(^ a b)` | `a ^ b` |
//! | `(<< a b)` | `a << b` |
//! | `(>> a b)` | `a >> b` |
//!
//! ## References, Casting & Misc
//! | S-expression | Rust output |
//! |---|---|
//! | `(ref mut x)` | `&mut x` |
//! | `(ref x)` | `&x` |
//! | `(deref x)` | `*x` |
//! | `(as x Type)` | `x as Type` |
//! | `(? x)` | `x?` |

//! | `(.. a b)` | `a..b` |
//! | `(..= a b)` | `a..=b` |
//! | `(.. a)` | `a..` |
//! | `(..)` | `..` |
//! | `(index coll key)` | `coll[key]` |
//! | `(. obj field)` | `obj.field` |
//! | `(. obj a b c)` | `obj.a.b.c` |

//! | `(new Name (f1 v1)...)` | `Name { f1: v1, ... }` |

//! | `(tuple a b c)` | `(a, b, c)` |
//! | `(tuple a)` | `(a,)` |
//! | `(tuple)` | `()` |
//! | `(array 1 2 3)` | `[1, 2, 3]` |
//! | `(vec a b c)` | `vec![a, b, c]` |
//! | `(val x)` | `x` |
//! | `(rust { code })` | `code` |
//! | `(rust stmt...)` | `stmt; ...` |
//! | `(path::func args...)` | `path::func(args...)` |
//! | `(obj.method args...)` | `obj.method(args...)` |
//! | `(func args...)` | `func(args...)` |
//!
//! ## Comments
//!
//! Use Rust-style `///` doc comments instead of Lisp-style `;` comments.
//! Place them inside `lisp!()` before item definitions:
//!
//! ```rust,ignore
//! lisp!(
//!     /// Adds two integers
//!     fn add ((a i32) (b i32)) i32
//!         (+ a b));
//! ```
//!
//! Between `lisp!()` invocations, use regular Rust `//` comments.

// Re-export proc macros so users see one unified crate.
pub use lisp_macro::{lisp_assign, lisp_eval, lisp_impl, lisp_trait, lisp_enum, lisp_struct, lisp_fn, lisp_let};

#[macro_export]
macro_rules! lisp {
    // ── Type & Item Definitions (unified visibility via $vis:vis) ──

    // struct — dispatch all forms to proc macro
    ( $(#[$m:meta])* $vis:vis struct $name:ident $($rest:tt)* ) => ( $crate::lisp_struct!($(#[$m])* $vis struct $name $($rest)*); );

    // enum (brace passthrough)
    ( $(#[$m:meta])* $vis:vis enum $name:ident { $($body:tt)* }) => ( $(#[$m]);* $vis enum $name { $($body)* } );
    // enum (S-expression variants — dispatched to proc macro)
    ( $(#[$m:meta])* $vis:vis enum $name:ident < $($rest:tt)+ ) => ( $crate::lisp_enum!($(#[$m])* $vis enum $name < $($rest)+); );
    ( $(#[$m:meta])* $vis:vis enum $name:ident $( ( $($variant:tt)+ ) )+ ) => ( $crate::lisp_enum!($(#[$m])* $vis enum $name $( ( $($variant)+ ) )+); );

    // trait — always dispatched to proc macro for proper fn handling
    ( $(#[$m:meta])* $vis:vis trait $name:ident $($rest:tt)* ) => ( $crate::lisp_trait!($vis $name $($rest)*); );

    // impl — always dispatched to proc macro for proper fn handling
    (impl $($tokens:tt)+) => ( $crate::lisp_impl!($($tokens)+); );

    // const fn — dispatch to proc macro (MUST precede const item rules)
    ( $(#[$m:meta])* $vis:vis const fn $sym:ident $($rest:tt)+ ) => ( $crate::lisp_fn!($(#[$m])* $vis const fn $sym $($rest)+); );

    // const, static, type — delegate to proc-macro for syn::Type validation.
    // The semicolons after lisp_eval! are needed for item-level macro invocations.
    ($vis:vis const $name:ident $($rest:tt)+) => ($crate::lisp_eval!($vis const $name $($rest)+););
    ($vis:vis static $($rest:tt)+) => ($crate::lisp_eval!($vis static $($rest)+););
    ($vis:vis type $name:ident = $($rest:tt)+) => ($crate::lisp_eval!($vis type $name = $($rest)+););

    // ── Imports & Modules ────────────────────────────────────

    // extern crate
    ( $(#[$m:meta])* extern crate $sym:ident) => ($(#[$m]);* extern crate $sym;);

    // use
    (use $sym:tt $(:: $sym2:tt)* ) => (use $sym $(:: $sym2)* ;);

    // mod — unified visibility
    ( $(#[$m:meta])* $vis:vis mod $sym:ident
        $( ( $($e:tt)* ))*
     ) => (
         $(#[$m]);*
         $vis mod $sym {
             $( $crate::lisp!( $($e)* ); )*
         }
    );

    // ── Closures (fn move ...) — must precede named fn arms since `move` matches $sym:ident ──
    // Closures starting with `fn (` naturally fall through to the catch-all because `(` is not an ident.
    // But `fn move ...` would match the named fn arm with $sym = move, so we intercept it here.
    (fn move $($rest:tt)+) => ($crate::lisp_eval!(fn move $($rest)+));

    // ── Named Functions (unified visibility + modifier dispatch to proc macro) ─
    // $vis:vis collapses pub / pub(crate) / pub(super) / empty into one rule per qualifier.
    ( $(#[$m:meta])* $vis:vis unsafe fn $sym:ident $($rest:tt)+ ) => ( $crate::lisp_fn!($(#[$m])* $vis unsafe fn $sym $($rest)+); );
    ( $(#[$m:meta])* $vis:vis async fn $sym:ident $($rest:tt)+ ) => ( $crate::lisp_fn!($(#[$m])* $vis async fn $sym $($rest)+); );
    ( $(#[$m:meta])* $vis:vis extern $abi:literal fn $sym:ident $($rest:tt)+ ) => ( $crate::lisp_fn!($(#[$m])* $vis extern $abi fn $sym $($rest)+); );
    ( $(#[$m:meta])* $vis:vis fn $sym:ident $($rest:tt)+ ) => ( $crate::lisp_fn!($(#[$m])* $vis fn $sym $($rest)+); );

    // ── Rust escape ──────────────────────────────────────────
    // Uses $st:stmt fragment for correct statement-level parsing.
    // Kept in macro_rules! because $st:stmt ensures proper statement boundaries
    // that raw token passthrough in the proc-macro cannot replicate.
    (rust { $($t:tt)* }) => ({ $($t)* });
    (rust $( $st:stmt )* ) => ( $($st);* );

    // ── Expression evaluation — delegated to lisp_eval! proc macro ──────────
    // All expression forms (arithmetic, comparison, logical, control flow,
    // bindings, references, collections, field access, function calls, etc.)
    // are handled by the single eval_lisp_expr engine in the proc macro.
    ($($t:tt)+) => ($crate::lisp_eval!($($t)+));
}
