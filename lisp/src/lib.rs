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

// Re-export proc macros so users see one unified crate.
pub use lisp_macro::{lisp_assign, lisp_impl, lisp_trait, lisp_enum, lisp_struct, lisp_fn, lisp_let};

#[macro_export]
macro_rules! lisp {
    // ── Literals ─────────────────────────────────────────────
    (false) => (false);
    (true) => (true);
    (self $(. $e:tt)* ) => (self $(. $e)* );

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

    // type alias
    ($vis:vis type $name:ident = $target:ty) => ($vis type $name = $target;);

    // const fn — dispatch to proc macro (MUST precede const variable rules)
    ( $(#[$m:meta])* $vis:vis const fn $sym:ident $($rest:tt)+ ) => ( $crate::lisp_fn!($(#[$m])* $vis const fn $sym $($rest)+); );

    // const and static (with = separator)
    ($vis:vis const $name:ident $typ:ty = $val:expr) => ($vis const $name: $typ = $val;);
    ($vis:vis static mut $name:ident $typ:ty = $val:expr) => ($vis static mut $name: $typ = $val;);
    ($vis:vis static $name:ident $typ:ty = $val:expr) => ($vis static $name: $typ = $val;);

    // const/static (without = separator)
    (const $name:ident $typ:tt $val:tt) => (const $name: $typ = $crate::lisp_arg!($val););
    (static mut $name:ident $typ:tt $val:tt) => (static mut $name: $typ = $crate::lisp_arg!($val););
    (static $name:ident $typ:tt $val:tt) => (static $name: $typ = $crate::lisp_arg!($val););

    // ── Match ────────────────────────────────────────────────
    // match with lisp body in parens
    (match $e:tt $( ( $pattern:pat_param $(| $pat2:pat_param)* => ( $($e2:tt)* ) ) )* ) => (
        match $crate::lisp_arg!($e) {
            $($pattern $(| $pat2)* => $crate::lisp_match_arg!($($e2)*) ),*
        }
    );
    // match with bare expression body (single token tree)
    (match $e:tt $( ( $pattern:pat_param $(| $pat2:pat_param)* => $body:tt ) )* ) => (
        match $crate::lisp_arg!($e) {
            $($pattern $(| $pat2)* => $crate::lisp_match_arg!($body) ),*
        }
    );

    // ── Bindings ─────────────────────────────────────────────

    // let mut
    (let mut ($var:ident $typ:ty) ( $($e:tt)+ )) => (let mut $var: $typ = $crate::lisp!($($e)+););
    (let mut ($var:ident $typ:ty) $e:expr) => (let mut $var: $typ = $e;);
    (let mut $var:ident ( $($e:tt)+ )) => (let mut $var = $crate::lisp!($($e)+););
    (let mut $var:ident $e:expr) => (let mut $var = $e;);

    // let (immutable)
    (let ($var:ident $typ:ty) ( $($e:tt)+ ) ) => (let $var: $typ = $crate::lisp!( $($e)+););
    (let ($var:ident $typ:ty) $e:expr) => (let $var: $typ = $e;);
    // let struct destructuring: (let Name { fields... } value)
    (let $name:ident { $($pat:tt)* } $e:tt) => (let $name { $($pat)* } = $crate::lisp_arg!($e););
    (let $var:ident ( $($e:tt)+ ) ) => (let $var = $crate::lisp!( $($e)+ ););
    (let $var:ident $e:expr) => (let $var = $e;);

    // let (scoped mutable bindings)
    (let ( $( ($var:ident $e:tt) )* )
        $( ( $($e2:tt)* ) )*
    ) => ({
        $(let mut $var = $crate::lisp_arg!($e);)*
        $( $crate::lisp!( $($e2)* ) );*
    });

    // let (pattern destructuring — fallback to proc macro)
    (let $($tokens:tt)+) => ($crate::lisp_let!($($tokens)+));

    // ── Assignment ───────────────────────────────────────────

    // = assignment
    (= $var:ident ( $($e:tt)+ )) => ($var = $crate::lisp!($($e)+););
    (= $var:ident $e:expr) => ($var = $e;);

    // compound assignment
    (+= $var:ident $e:tt) => ($var += $crate::lisp_arg!($e););
    (-= $var:ident $e:tt) => ($var -= $crate::lisp_arg!($e););
    (*= $var:ident $e:tt) => ($var *= $crate::lisp_arg!($e););
    (/= $var:ident $e:tt) => ($var /= $crate::lisp_arg!($e););
    (%= $var:ident $e:tt) => ($var %= $crate::lisp_arg!($e););
    (&= $var:ident $e:tt) => ($var &= $crate::lisp_arg!($e););
    (|= $var:ident $e:tt) => ($var |= $crate::lisp_arg!($e););
    (^= $var:ident $e:tt) => ($var ^= $crate::lisp_arg!($e););
    (<<= $var:ident $e:tt) => ($var <<= $crate::lisp_arg!($e););
    (>>= $var:ident $e:tt) => ($var >>= $crate::lisp_arg!($e););

    // assignment (arbitrary LHS — dispatched to proc macro)
    (= $($tokens:tt)+) => ($crate::lisp_assign!(= $($tokens)+));
    (+= $($tokens:tt)+) => ($crate::lisp_assign!(+= $($tokens)+));
    (-= $($tokens:tt)+) => ($crate::lisp_assign!(-= $($tokens)+));
    (*= $($tokens:tt)+) => ($crate::lisp_assign!(*= $($tokens)+));
    (/= $($tokens:tt)+) => ($crate::lisp_assign!(/= $($tokens)+));
    (%= $($tokens:tt)+) => ($crate::lisp_assign!(%= $($tokens)+));
    (&= $($tokens:tt)+) => ($crate::lisp_assign!(&= $($tokens)+));
    (|= $($tokens:tt)+) => ($crate::lisp_assign!(|= $($tokens)+));
    (^= $($tokens:tt)+) => ($crate::lisp_assign!(^= $($tokens)+));
    (<<= $($tokens:tt)+) => ($crate::lisp_assign!(<<= $($tokens)+));
    (>>= $($tokens:tt)+) => ($crate::lisp_assign!(>>= $($tokens)+));

    // ── Block ────────────────────────────────────────────────
    (block $l:lifetime $( ( $($e:tt)* ) )* ) => ($l: { $( $crate::lisp!($($e)*) );* });
    (block $( ( $($e:tt)* ) )* ) => ({ $( $crate::lisp!($($e)*) );* });

    // ── Loops ────────────────────────────────────────────────
    (break $l:lifetime $e:tt) => (break $l $crate::lisp_arg!($e););
    (break $l:lifetime) => (break $l;);
    (break $e:tt) => (break $crate::lisp_arg!($e););
    (break) => (break;);
    (continue $l:lifetime) => (continue $l;);
    (continue) => (continue;);

    // labeled loops
    ($l:lifetime loop $( ( $($e:tt)* ) )* ) => ( $l: loop { $( $crate::lisp!( $($e)* ) );* });
    ($l:lifetime while let ( $pattern:pat = $($cond:tt)* ) $( ( $($e:tt)* ) )* ) => ( $l: while let $pattern = $crate::lisp_arg!($($cond)*) { $( $crate::lisp!($($e)*) );* });
    ($l:lifetime while $cond:tt $( ( $($e:tt)* ) )* ) => ( $l: while $crate::lisp_arg!($cond) { $( $crate::lisp!( $($e)* ) );* });
    ($l:lifetime for $var:ident in $iter:tt $( ( $($e:tt)* ) )* ) => ( $l: for $var in $crate::lisp_arg!($iter) { $( $crate::lisp!($($e)*) );* } );

    (loop $( ( $($e:tt)* ) )* ) => ( loop { $( $crate::lisp!( $($e)* ) );* });

    // while let (BEFORE while)
    (while let ( $pattern:pat = $($cond:tt)* ) $( ( $($e:tt)* ) )* ) => ( while let $pattern = $crate::lisp_arg!($($cond)*) { $( $crate::lisp!($($e)*) );* });

    // while
    (while $cond:tt $( ( $($e:tt)* ) )* ) => ( while $crate::lisp_arg!($cond) { $( $crate::lisp!( $($e)* ) );* });

    // for...in
    (for $var:ident in $iter:tt $( ( $($e:tt)* ) )* ) => ( for $var in $crate::lisp_arg!($iter) { $( $crate::lisp!($($e)*) );* } );

    // ── Conditionals ─────────────────────────────────────────

    // if let (BEFORE if)
    (if let ( $pattern:pat = $($cond:tt)* ) $e1:tt $e2:tt) => (if let $pattern = $crate::lisp_arg!($($cond)*) { $crate::lisp_arg!($e1) } else { $crate::lisp_arg!($e2) });
    (if let ( $pattern:pat = $($cond:tt)* ) $e:tt) => (if let $pattern = $crate::lisp_arg!($($cond)*) { $crate::lisp_arg!($e) });

    // if
    (if ( $($cond:tt)* ) $e1:tt $e2:tt) => (if $crate::lisp!($($cond)*) { $crate::lisp_arg!($e1) }else{ $crate::lisp_arg!($e2) });
    (if ( $($cond:tt)* ) $e:tt) => (if $crate::lisp!($($cond)*) { $crate::lisp_arg!($e) });
    (if $cond:tt $e1:tt $e2:tt) => (if $cond { $crate::lisp_arg!($e1) }else{ $crate::lisp_arg!($e2) });
    (if $cond:tt $e:tt) => (if $cond { $crate::lisp_arg!($e) });

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

    // ── Closures ─────────────────────────────────────────────

    // closure (fn move) — untyped params
    (fn move ( $( ( $name:ident ) )+ )
        $( ( $($e:tt)* ))*
    ) => (move | $($name),* |{ $( $crate::lisp!( $($e)* ) );* });

    // closure (fn move)
    (fn move ( $( ( $name:ident $typ:ty ) )* )
        $( ( $($e:tt)* ))*
    ) => (move | $($name : $typ),* |{ $( $crate::lisp!( $($e)* ) );* });

    // closure (fn) — untyped params
    (fn ( $( ( $name:ident ) )+ )
        $( ( $($e:tt)* ))*
    ) => (| $($name),* |{ $( $crate::lisp!( $($e)* ) );* });

    // closure (fn)
    (fn ( $( ( $name:ident $typ:ty ) )* )
        $( ( $($e:tt)* ))*
    ) => (| $($name : $typ),* |{ $( $crate::lisp!( $($e)* ) );* });

    // ── Named Functions (unified visibility + modifier dispatch to proc macro) ─
    // Closures above are matched first since they start with `fn (` not `fn $name`.
    // $vis:vis collapses pub / pub(crate) / pub(super) / empty into one rule per qualifier.
    ( $(#[$m:meta])* $vis:vis unsafe fn $sym:ident $($rest:tt)+ ) => ( $crate::lisp_fn!($(#[$m])* $vis unsafe fn $sym $($rest)+); );
    ( $(#[$m:meta])* $vis:vis async fn $sym:ident $($rest:tt)+ ) => ( $crate::lisp_fn!($(#[$m])* $vis async fn $sym $($rest)+); );
    ( $(#[$m:meta])* $vis:vis extern $abi:literal fn $sym:ident $($rest:tt)+ ) => ( $crate::lisp_fn!($(#[$m])* $vis extern $abi fn $sym $($rest)+); );
    ( $(#[$m:meta])* $vis:vis fn $sym:ident $($rest:tt)+ ) => ( $crate::lisp_fn!($(#[$m])* $vis fn $sym $($rest)+); );

    // ── Return ───────────────────────────────────────────────
    (return ( $($e:tt)+ )) => (return $crate::lisp!($($e)+));
    (return $e:tt) => (return $crate::lisp_arg!($e));
    (return) => (return);

    // ── Unsafe Block ─────────────────────────────────────────
    (unsafe $( ( $($e:tt)* ) )* ) => (unsafe { $( $crate::lisp!($($e)*) );* });

    // ── Await ────────────────────────────────────────────────
    (await $e:tt) => ($crate::lisp_arg!($e).await);

    // ── Comparison ───────────────────────────────────────────
    (== $x:tt $y:tt) => ($crate::lisp_arg!($x) == $crate::lisp_arg!($y));
    (!= $x:tt $y:tt) => ($crate::lisp_arg!($x) != $crate::lisp_arg!($y));
    (< $x:tt $y:tt) => ($crate::lisp_arg!($x) < $crate::lisp_arg!($y));
    (> $x:tt $y:tt) => ($crate::lisp_arg!($x) > $crate::lisp_arg!($y));
    (<= $x:tt $y:tt) => ($crate::lisp_arg!($x) <= $crate::lisp_arg!($y));
    (>= $x:tt $y:tt) => ($crate::lisp_arg!($x) >= $crate::lisp_arg!($y));

    // ── Output ───────────────────────────────────────────────
    (panic $($arg:tt)+ ) => ( panic!( $($arg)+ ); );

    // ── Logical ──────────────────────────────────────────────
    (! $e:tt) => ( ! $crate::lisp_arg!($e));
    (&& $x:tt $y:tt) => ($crate::lisp_arg!($x) && $crate::lisp_arg!($y));
    (|| $x:tt $y:tt) => ($crate::lisp_arg!($x) || $crate::lisp_arg!($y));

    // ── Arithmetic ───────────────────────────────────────────
    (+ $a:tt $b:tt $($rest:tt)+) => ($crate::lisp!(+ {$crate::lisp_arg!($a) + $crate::lisp_arg!($b)} $($rest)+));
    (+ $x:tt $y:tt) => ($crate::lisp_arg!($x) + $crate::lisp_arg!($y));
    (- $a:tt $b:tt $($rest:tt)+) => ($crate::lisp!(- {$crate::lisp_arg!($a) - $crate::lisp_arg!($b)} $($rest)+));
    (- $x:tt $y:tt) => ($crate::lisp_arg!($x) - $crate::lisp_arg!($y));
    (* $a:tt $b:tt $($rest:tt)+) => ($crate::lisp!(* {$crate::lisp_arg!($a) * $crate::lisp_arg!($b)} $($rest)+));
    (* $x:tt $y:tt) => ($crate::lisp_arg!($x) * $crate::lisp_arg!($y));
    (/ $a:tt $b:tt $($rest:tt)+) => ($crate::lisp!(/ {$crate::lisp_arg!($a) / $crate::lisp_arg!($b)} $($rest)+));
    (/ $x:tt $y:tt) => ($crate::lisp_arg!($x) / $crate::lisp_arg!($y));
    (% $x:tt $y:tt) => ($crate::lisp_arg!($x) % $crate::lisp_arg!($y));

    // ── Negation ─────────────────────────────────────────────
    (neg $e:tt) => (- $crate::lisp_arg!($e));

    // ── Bitwise ──────────────────────────────────────────────
    (& $a:tt $b:tt $($rest:tt)+) => ($crate::lisp!(& {$crate::lisp_arg!($a) & $crate::lisp_arg!($b)} $($rest)+));
    (& $a:tt $b:tt) => ($crate::lisp_arg!($a) & $crate::lisp_arg!($b));
    (| $a:tt $b:tt $($rest:tt)+) => ($crate::lisp!(| {$crate::lisp_arg!($a) | $crate::lisp_arg!($b)} $($rest)+));
    (| $a:tt $b:tt) => ($crate::lisp_arg!($a) | $crate::lisp_arg!($b));
    (^ $a:tt $b:tt $($rest:tt)+) => ($crate::lisp!(^ {$crate::lisp_arg!($a) ^ $crate::lisp_arg!($b)} $($rest)+));
    (^ $a:tt $b:tt) => ($crate::lisp_arg!($a) ^ $crate::lisp_arg!($b));
    (<< $a:tt $b:tt) => ($crate::lisp_arg!($a) << $crate::lisp_arg!($b));
    (>> $a:tt $b:tt) => ($crate::lisp_arg!($a) >> $crate::lisp_arg!($b));

    // ── References & Casting ─────────────────────────────────
    (ref mut $e:tt) => (&mut $crate::lisp_arg!($e));
    (ref $e:tt) => (&$crate::lisp_arg!($e));
    (deref $e:tt) => (*$crate::lisp_arg!($e));
    (as $e:tt $typ:ty) => ($crate::lisp_arg!($e) as $typ);

    // ── Try ──────────────────────────────────────────────────
    (? $e:tt) => ($crate::lisp_arg!($e)?);

    // ── Range ────────────────────────────────────────────────
    (.. $a:tt $b:tt) => ($crate::lisp_arg!($a)..$crate::lisp_arg!($b));
    (..= $a:tt $b:tt) => ($crate::lisp_arg!($a)..=$crate::lisp_arg!($b));
    (.. $a:tt) => ($crate::lisp_arg!($a)..);
    (..) => (..);

    // ── Index ────────────────────────────────────────────────
    (index $coll:tt $key:tt) => ($crate::lisp_arg!($coll)[$crate::lisp_arg!($key)]);

    // ── Field ────────────────────────────────────────────────
    // . field access (chained)
    (. $obj:tt $( $name:ident )+) => ($crate::lisp_arg!($obj) $(.$name)+);

    // ── Construction ─────────────────────────────────────────
    // new (path-qualified struct/enum variant construction with spread)
    (new $name:ident $(:: $name2:ident)+ $( ($field:ident $val:tt) )* (.. $base:expr) ) => ( $name $(:: $name2)+ { $( $field: $crate::lisp_arg!($val), )* ..$base } );
    // new (path-qualified struct/enum variant construction)
    (new $name:ident $(:: $name2:ident)+ $( ($field:ident $val:tt) )* ) => ( $name $(:: $name2)+ { $( $field: $crate::lisp_arg!($val) ),* } );
    // new (path-qualified shorthand)
    (new $name:ident $(:: $name2:ident)+ $( $field:ident )+ ) => ( $name $(:: $name2)+ { $( $field ),* } );
    // new (struct construction)
    (new $name:ident $( ($field:ident $val:tt) )* (.. $base:expr) ) => ( $name { $( $field: $crate::lisp_arg!($val), )* ..$base } );
    (new $name:ident $( ($field:ident $val:tt) )* ) => ( $name { $( $field: $crate::lisp_arg!($val) ),* } );
    (new $name:ident $( $field:ident )+ ) => ( $name { $( $field ),* } );
    // new (tuple struct construction): (new Name val1 val2) → Name(val1, val2)
    (new $name:ident $(:: $name2:ident)+ $($e:tt)+) => ( $name $(:: $name2)+ ( $($crate::lisp_arg!($e)),* ) );
    (new $name:ident $($e:tt)+) => ( $name ( $($crate::lisp_arg!($e)),* ) );
    // ── Len ──────────────────────────────────────────────────

    // ── Collections ──────────────────────────────────────────
    (tuple $single:tt) => (($crate::lisp_arg!($single),));
    (tuple $($e:tt)* ) => ( ($($crate::lisp_arg!($e)),*) );
    (vec $($e:tt)* ) => ( vec![$($crate::lisp_arg!($e)),*] );
    (array - repeat $val:tt $count:tt) => ([$crate::lisp_arg!($val); $crate::lisp_arg!($count)]);
    (array $($e:tt)* ) => ( [$($crate::lisp_arg!($e)),*] );

    // ── Val ──────────────────────────────────────────────────
    (val $e:tt) => ($crate::lisp_arg!($e));

    // ── Rust escape ──────────────────────────────────────────
    (rust { $($t:tt)* }) => ({ $($t)* });
    (rust $( $st:stmt )* ) => ( $($st);* );

    // ── Macro invocation ─────────────────────────────────────
    (macro ! $name:ident $(:: $name2:ident)* $($args:tt)*) => ($name $(:: $name2)* ! ($($crate::lisp_arg!($args)),*));

    // ── Macro invocation shorthand (ident! args...) ──────────
    ( $sym:ident $(:: $sym2:ident )+ ! $($args:tt)* ) => ( $sym $(:: $sym2 )+ ! ($($crate::lisp_arg!($args)),*) );
    ( $name:ident ! $($args:tt)* ) => ( $name ! ($($crate::lisp_arg!($args)),*) );

    // ── Catch-all ────────────────────────────────────────────
    ( $sym:ident $(:: $sym2:ident )+ $( $e:tt )* ) => ( $sym $(:: $sym2 )+ ( $($crate::lisp_arg!($e)),* ) );
    ( $sym:ident . $( $sym2:ident ).+ $( $e:tt )* ) => ( $sym.$( $sym2 ).+ ( $($crate::lisp_arg!($e)),* ) );
    ( $sym:ident $( $e:tt )* ) => ( $sym ( $($crate::lisp_arg!($e)),* ) );

    ($e:expr) => ($e);
}

#[macro_export]
macro_rules! lisp_arg {
    ( ( $($e:tt)* ) ) => ( $crate::lisp!( $($e)* ) );
    ($e:expr) => ($e);
}

#[macro_export]
macro_rules! lisp_match_arg {
    ($name:ident ! $($args:tt)*) => ($name ! ($($crate::lisp_arg!($args)),*));
    ($e:expr) => ($e);
    ( $($e:tt)* ) => ($crate::lisp!( $($e)* ));
}
