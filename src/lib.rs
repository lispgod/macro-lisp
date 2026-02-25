//! `lisp!` — Write Rust code in S-expression (Lisp-like) syntax.
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
//! | `(set var (expr))` | `var = expr;` |
//! | `(set var val)` | `var = val;` |
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
//! | `(when cond expr)` | `if cond { expr }` |
//! | `(unless cond expr)` | `if !cond { expr }` |
//!
//! ## Imports & Modules
//! | S-expression | Rust output |
//! |---|---|
//! | `(extern crate name)` | `extern crate name;` |
//! | `(use path::to::item)` | `use path::to::item;` |
//! | `(pub mod name (body)...)` | `pub mod name { body }` |
//! | `(mod name (body)...)` | `mod name { body }` |
//! | `(pub module name (body)...)` | `pub mod name { body }` |
//! | `(module name (body)...)` | `mod name { body }` |
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
//! | `(assert eq x y)` | `assert_eq!(x, y);` |
//! | `(debug assert eq x y)` | `debug_assert_eq!(x, y);` |
//! | `(debug assert x msg)` | `debug_assert!(x, msg);` |
//! | `(assert x msg)` | `assert!(x, msg);` |
//! | `(print fmt args...)` | `print!(fmt, args...)` |
//! | `(println fmt args...)` | `println!(fmt, args...)` |
//! | `(format fmt args...)` | `format!(fmt, args...)` |
//! | `(panic args...)` | `panic!(args...)` |
//! | `(macro! name args...)` | `name!(args...)` |
//!
//! ## Logical Operators
//! | S-expression | Rust output |
//! |---|---|
//! | `(and a b ...)` | `a && b && ...` |
//! | `(or a b ...)` | `a \|\| b \|\| ...` |
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
//! | `(range a b)` | `a..b` |
//! | `(range= a b)` | `a..=b` |
//! | `(.. a b)` | `a..b` |
//! | `(..= a b)` | `a..=b` |
//! | `(.. a)` | `a..` |
//! | `(..)` | `..` |
//! | `(index coll key)` | `coll[key]` |
//! | `(. obj field)` | `obj.field` |
//! | `(. obj a b c)` | `obj.a.b.c` |
//! | `(field obj name)` | `obj.name` |
//! | `(struct-lit Name (f1 v1)...)` | `Name { f1: v1, ... }` |
//! | `(new Name (f1 v1)...)` | `Name { f1: v1, ... }` |
//! | `(box x)` | `Box::new(x)` |
//! | `(len x)` | `x.len()` |
//! | `(tuple a b c)` | `(a, b, c)` |
//! | `(tuple a)` | `(a,)` |
//! | `(tuple)` | `()` |
//! | `(array 1 2 3)` | `[1, 2, 3]` |
//! | `(array-repeat 0 10)` | `[0; 10]` |
//! | `(vec a b c)` | `vec![a, b, c]` |
//! | `(val x)` | `x` |
//! | `(rust { code })` | `code` |
//! | `(rust stmt...)` | `stmt; ...` |
//! | `(path::func args...)` | `path::func(args...)` |
//! | `(obj.method args...)` | `obj.method(args...)` |
//! | `(func args...)` | `func(args...)` |

#[macro_export]
macro_rules! lisp {
    // ── Literals ─────────────────────────────────────────────
    (false) => (false);
    (true) => (true);
    (self $(. $e:tt)* ) => (self $(. $e)* );

    // ── Type & Item Definitions ──────────────────────────────

    // pub struct (with generics, pub + private fields)
    ( $(#[$m:meta])* pub struct $struct_name:ident < $($generic:ident),+ >
        (pub $( ($name:ident $typ:ty) )* )
        ( $( ($name2:ident $typ2:ty) )* )
    ) => (
        $(#[$m]);*
        pub struct $struct_name < $($generic),+ > {
            $( pub $name: $typ),*
            ,
            $( $name2: $typ2),*
        }
    );
    // pub struct (with generics, pub fields only)
    ( $(#[$m:meta])* pub struct $struct_name:ident < $($generic:ident),+ >
        (pub $( ($name:ident $typ:ty) )* )
    ) => (
        $(#[$m]);*
        pub struct $struct_name < $($generic),+ > {
            $( pub $name: $typ),*
        }
    );
    // pub struct (with generics, private fields only)
    ( $(#[$m:meta])* pub struct $struct_name:ident < $($generic:ident),+ >
        ( $( ($name:ident $typ:ty) )* )
    ) => (
        $(#[$m]);*
        pub struct $struct_name < $($generic),+ > {
            $( $name: $typ ),*
        }
    );
    // pub struct (no generics, pub + private fields)
    ( $(#[$m:meta])* pub struct $struct_name:ident
        (pub $( ($name:ident $typ:ty) )* )
        ( $( ($name2:ident $typ2:ty) )* )
    ) => (
        $(#[$m]);*
        pub struct $struct_name {
            $( pub $name: $typ),*
            ,
            $( $name2: $typ2),*
        }
    );
    // pub struct (no generics, pub fields only)
    ( $(#[$m:meta])* pub struct $struct_name:ident
        (pub $( ($name:ident $typ:ty) )* )
    ) => (
        $(#[$m]);*
        pub struct $struct_name {
            $( pub $name: $typ),*
        }
    );
    // pub struct (no generics, private fields only)
    ( $(#[$m:meta])* pub struct $struct_name:ident
        ( $( ($name:ident $typ:ty) )* )
    ) => (
        $(#[$m]);*
        pub struct $struct_name {
            $( $name: $typ ),*
        }
    );
    // pub struct (unit)
    ( $(#[$m:meta])* pub struct $struct_name:ident) => (
        $(#[$m]);*
        pub struct $struct_name;
    );

    // struct (with generics, pub + private fields)
    ( $(#[$m:meta])* struct $struct_name:ident < $($generic:ident),+ >
        (pub $( ($name:ident $typ:ty) )* )
        ( $( ($name2:ident $typ2:ty) )* )
    ) => (
        $(#[$m]);*
        struct $struct_name < $($generic),+ > {
            $( pub $name: $typ),*
            ,
            $( $name2: $typ2),*
        }
    );
    // struct (with generics, pub fields only)
    ( $(#[$m:meta])* struct $struct_name:ident < $($generic:ident),+ >
        (pub $( ($name:ident $typ:ty) )* )
    ) => (
        $(#[$m]);*
        struct $struct_name < $($generic),+ > {
            $( pub $name: $typ),*
        }
    );
    // struct (with generics, private fields only)
    ( $(#[$m:meta])* struct $struct_name:ident < $($generic:ident),+ >
        ( $( ($name:ident $typ:ty) )* )
    ) => (
        $(#[$m]);*
        struct $struct_name < $($generic),+ > {
            $( $name: $typ ),*
        }
    );
    // struct (no generics, pub + private fields)
    ( $(#[$m:meta])* struct $struct_name:ident
        (pub $( ($name:ident $typ:ty) )* )
        ( $( ($name2:ident $typ2:ty) )* )
    ) => (
        $(#[$m]);*
        struct $struct_name {
            $( pub $name: $typ),*
            ,
            $( $name2: $typ2),*
        }
    );
    // struct (no generics, pub fields only)
    ( $(#[$m:meta])* struct $struct_name:ident
        (pub $( ($name:ident $typ:ty) )* )
    ) => (
        $(#[$m]);*
        struct $struct_name {
            $( pub $name: $typ),*
        }
    );
    // struct (no generics, private fields only)
    ( $(#[$m:meta])* struct $struct_name:ident
        ( $( ($name:ident $typ:ty) )* )
    ) => (
        $(#[$m]);*
        struct $struct_name {
            $( $name: $typ ),*
        }
    );
    // struct (unit)
    ( $(#[$m:meta])* struct $struct_name:ident) => (
        $(#[$m]);*
        struct $struct_name;
    );

    // pub struct (tuple, with generics)
    ( $(#[$m:meta])* pub struct $struct_name:ident < $($generic:ident),+ >
        ( $( $typ:ty )* )
    ) => (
        $(#[$m]);*
        pub struct $struct_name < $($generic),+ > ( $($typ),* );
    );
    // pub struct (tuple, no generics)
    ( $(#[$m:meta])* pub struct $struct_name:ident
        ( $( $typ:ty )* )
    ) => (
        $(#[$m]);*
        pub struct $struct_name ( $($typ),* );
    );
    // struct (tuple, with generics)
    ( $(#[$m:meta])* struct $struct_name:ident < $($generic:ident),+ >
        ( $( $typ:ty )* )
    ) => (
        $(#[$m]);*
        struct $struct_name < $($generic),+ > ( $($typ),* );
    );
    // struct (tuple, no generics)
    ( $(#[$m:meta])* struct $struct_name:ident
        ( $( $typ:ty )* )
    ) => (
        $(#[$m]);*
        struct $struct_name ( $($typ),* );
    );

    // enum
    ( $(#[$m:meta])* pub enum $name:ident { $($body:tt)* }) => ( $(#[$m]);* pub enum $name { $($body)* } );
    ( $(#[$m:meta])* enum $name:ident { $($body:tt)* }) => ( $(#[$m]);* enum $name { $($body)* } );

    // trait
    ( $(#[$m:meta])* pub trait $name:ident $( ( $($e:tt)* ) )* ) => ( $(#[$m]);* pub trait $name { $( $crate::lisp!{$($e)*} )* } );
    ( $(#[$m:meta])* trait $name:ident $( ( $($e:tt)* ) )* ) => ( $(#[$m]);* trait $name { $( $crate::lisp!{$($e)*} )* } );

    // impl
    (impl $trait_name:ident for $typ:ident $( ( $($e:tt)* ) )* ) => ( impl $trait_name for $typ { $( $crate::lisp!{$($e)*} )* } );
    (impl $typ:ident $( ( $($e:tt)* ) )* ) => ( impl $typ { $( $crate::lisp!{$($e)*} )* } );

    // type alias
    (pub type $name:ident = $target:ty) => (pub type $name = $target;);
    (type $name:ident = $target:ty) => (type $name = $target;);

    // const fn (MUST come before const variable to avoid :ident matching `fn` keyword)
    // const fn & return
    ( $(#[$m:meta])* const fn $sym:ident ( $( ( $name:ident $typ:ty ) )* ) $return_type:tt
        $( ( $($e:tt)* ))*
    ) => (
        $(#[$m]);*
        const fn $sym( $($name : $typ),* ) -> $return_type {
            $( $crate::lisp!( $($e)* ) );*
        }
    );
    // const fn & void
    ( $(#[$m:meta])* const fn $sym:ident ( $( ( $name:ident $typ:ty ) )* )
        $( ( $($e:tt)* ))*
    ) => (
        $(#[$m]);*
        const fn $sym( $($name : $typ),* ) {
            $( $crate::lisp!( $($e)* ) );*
        }
    );
    // pub const fn & return
    ( $(#[$m:meta])* pub const fn $sym:ident ( $( ( $name:ident $typ:ty ) )* ) $return_type:tt
        $( ( $($e:tt)* ))*
    ) => (
        $(#[$m]);*
        pub const fn $sym( $($name : $typ),* ) -> $return_type {
            $( $crate::lisp!( $($e)* ) );*
        }
    );
    // pub const fn & void
    ( $(#[$m:meta])* pub const fn $sym:ident ( $( ( $name:ident $typ:ty ) )* )
        $( ( $($e:tt)* ))*
    ) => (
        $(#[$m]);*
        pub const fn $sym( $($name : $typ),* ) {
            $( $crate::lisp!( $($e)* ) );*
        }
    );

    // const and static
    (pub const $name:ident $typ:ty = $val:expr) => (pub const $name: $typ = $val;);
    (const $name:ident $typ:ty = $val:expr) => (const $name: $typ = $val;);
    (static mut $name:ident $typ:ty = $val:expr) => (static mut $name: $typ = $val;);
    (static $name:ident $typ:ty = $val:expr) => (static $name: $typ = $val;);

    // const/static (without = separator)
    (const $name:ident $typ:tt $val:tt) => (const $name: $typ = $crate::lisp_arg!($val););
    (static mut $name:ident $typ:tt $val:tt) => (static mut $name: $typ = $crate::lisp_arg!($val););
    (static $name:ident $typ:tt $val:tt) => (static $name: $typ = $crate::lisp_arg!($val););

    // ── Match ────────────────────────────────────────────────
    (match $e:tt $( ( $pattern:pat_param $(| $pat2:pat_param)* => ( $($e2:tt)* ) ) )* ) => (
        match $crate::lisp_arg!($e) {
            $($pattern $(| $pat2)* => $crate::lisp_match_arg!($($e2)*) ),*
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
    (let $var:ident ( $($e:tt)+ ) ) => (let $var = $crate::lisp!( $($e)+ ););
    (let $var:ident $e:expr) => (let $var = $e;);

    // let (scoped mutable bindings)
    (let ( $( ($var:ident $e:tt) )* )
        $( ( $($e2:tt)* ) )*
    ) => ({
        $(let mut $var = $crate::lisp_arg!($e);)*
        $( $crate::lisp!( $($e2)* ) );*
    });

    // ── Assignment ───────────────────────────────────────────

    // set
    (set $var:ident ( $($e:tt)+ )) => ($var = $crate::lisp!($($e)+););
    (set $var:ident $e:expr) => ($var = $e;);

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

    // when, unless
    (when ( $($cond:tt)* ) $e:tt) => (if $crate::lisp!($($cond)*) { $crate::lisp_arg!($e) });
    (when $cond:tt $e:tt) => (if $cond { $crate::lisp_arg!($e) });
    (unless ( $($cond:tt)* ) $e:tt) => (if ! ($crate::lisp!($($cond)*)) { $crate::lisp_arg!($e) });
    (unless $cond:tt $e:tt) => (if !($cond) { $crate::lisp_arg!($e) });

    // ── Imports & Modules ────────────────────────────────────

    // extern crate
    ( $(#[$m:meta])* extern crate $sym:ident) => ($(#[$m]);* extern crate $sym;);

    // use
    (use $sym:tt $(:: $sym2:tt)* ) => (use $sym $(:: $sym2)* ;);

    // module
    ( $(#[$m:meta])* pub module $sym:ident
        $( ( $($e:tt)* ))*
     ) => (
         $(#[$m]);*
         pub mod $sym {
             $( $crate::lisp!( $($e)* ); )*
         }
    );
    ( $(#[$m:meta])* module $sym:ident
        $( ( $($e:tt)* ))*
     ) => (
         $(#[$m]);*
         mod $sym {
             $( $crate::lisp!( $($e)* ); )*
         }
    );

    // mod (alias for module)
    ( $(#[$m:meta])* pub mod $sym:ident
        $( ( $($e:tt)* ))*
     ) => (
         $(#[$m]);*
         pub mod $sym {
             $( $crate::lisp!( $($e)* ); )*
         }
    );
    ( $(#[$m:meta])* mod $sym:ident
        $( ( $($e:tt)* ))*
     ) => (
         $(#[$m]);*
         mod $sym {
             $( $crate::lisp!( $($e)* ); )*
         }
    );

    // ── Functions & Closures ─────────────────────────────────

    // pub unsafe fn & return
    ( $(#[$m:meta])* pub unsafe fn $sym:ident ( $( ( $name:ident $typ:ty ) )* ) $return_type:tt
        $( ( $($e:tt)* ))*
    ) => (
        $(#[$m]);*
        pub unsafe fn $sym( $($name : $typ),* ) -> $return_type {
            $( $crate::lisp!( $($e)* ) );*
        }
    );
    // pub unsafe fn & void
    ( $(#[$m:meta])* pub unsafe fn $sym:ident ( $( ( $name:ident $typ:ty ) )* )
        $( ( $($e:tt)* ))*
    ) => (
        $(#[$m]);*
        pub unsafe fn $sym( $($name : $typ),* ) {
            $( $crate::lisp!( $($e)* ) );*
        }
    );

    // pub extern fn & return
    ( $(#[$m:meta])* pub extern $abi:literal fn $sym:ident ( $( ( $name:ident $typ:ty ) )* ) $return_type:tt
        $( ( $($e:tt)* ))*
    ) => (
        $(#[$m]);*
        pub extern $abi fn $sym( $($name : $typ),* ) -> $return_type {
            $( $crate::lisp!( $($e)* ) );*
        }
    );
    // pub extern fn & void
    ( $(#[$m:meta])* pub extern $abi:literal fn $sym:ident ( $( ( $name:ident $typ:ty ) )* )
        $( ( $($e:tt)* ))*
    ) => (
        $(#[$m]);*
        pub extern $abi fn $sym( $($name : $typ),* ) {
            $( $crate::lisp!( $($e)* ) );*
        }
    );

    // closure (fn move)
    (fn move ( $( ( $name:ident $typ:ty ) )* )
        $( ( $($e:tt)* ))*
    ) => (move | $($name : $typ),* |{ $( $crate::lisp!( $($e)* ) );* });

    // closure (fn)
    (fn ( $( ( $name:ident $typ:ty ) )* )
        $( ( $($e:tt)* ))*
    ) => (| $($name : $typ),* |{ $( $crate::lisp!( $($e)* ) );* });

    // async fn & return
    ( $(#[$m:meta])* async fn $sym:ident ( $( ( $name:ident $typ:ty ) )* ) $return_type:tt
        $( ( $($e:tt)* ))*
    ) => (
        $(#[$m]);*
        async fn $sym( $($name : $typ),* ) -> $return_type {
            $( $crate::lisp!( $($e)* ) );*
        }
    );
    // async fn & void
    ( $(#[$m:meta])* async fn $sym:ident ( $( ( $name:ident $typ:ty ) )* )
        $( ( $($e:tt)* ))*
    ) => (
        $(#[$m]);*
        async fn $sym( $($name : $typ),* ) {
            $( $crate::lisp!( $($e)* ) );*
        }
    );

    // pub fn & return
    ( $(#[$m:meta])* pub fn $sym:ident ( $( ( $name:ident $typ:ty ) )* ) $return_type:tt
        $( ( $($e:tt)* ))*
    ) => (
        $(#[$m]);*
        pub fn $sym( $($name : $typ),* ) -> $return_type {
            $( $crate::lisp!( $($e)* ) );*
        }
    );
    // pub fn & void
    ( $(#[$m:meta])* pub fn $sym:ident ( $( ( $name:ident $typ:ty ) )* )
        $( ( $($e:tt)* ))*
    ) => (
        $(#[$m]);*
        pub fn $sym( $($name : $typ),* ) {
            $( $crate::lisp!( $($e)* ) );*
        }
    );

    // fn & return
    ( $(#[$m:meta])* fn $sym:ident ( $( ( $name:ident $typ:ty ) )* ) $return_type:tt
        $( ( $($e:tt)* ))*
    ) => (
        $(#[$m]);*
        fn $sym ( $($name : $typ),* ) -> $return_type {
            $( $crate::lisp!( $($e)* ) );*
        }
    );
    // fn & void
    ( $(#[$m:meta])* fn $sym:ident ( $( ( $name:ident $typ:ty ) )* )
        $( ( $($e:tt)* ))*
    ) => (
        $(#[$m]);*
        fn $sym( $($name : $typ),* ) {
            $( $crate::lisp!( $($e)* ) );*
        }
    );

    // ── Return ───────────────────────────────────────────────
    (return ( $($e:tt)+ )) => (return $crate::lisp!($($e)+));
    (return $e:tt) => (return $crate::lisp_arg!($e));
    (return) => (return);

    // ── Unsafe ───────────────────────────────────────────────
    // unsafe fn & return
    ( $(#[$m:meta])* unsafe fn $sym:ident ( $( ( $name:ident $typ:ty ) )* ) $return_type:tt
        $( ( $($e:tt)* ))*
    ) => (
        $(#[$m]);*
        unsafe fn $sym( $($name : $typ),* ) -> $return_type {
            $( $crate::lisp!( $($e)* ) );*
        }
    );
    // unsafe fn & void
    ( $(#[$m:meta])* unsafe fn $sym:ident ( $( ( $name:ident $typ:ty ) )* )
        $( ( $($e:tt)* ))*
    ) => (
        $(#[$m]);*
        unsafe fn $sym( $($name : $typ),* ) {
            $( $crate::lisp!( $($e)* ) );*
        }
    );
    // extern fn & return
    ( $(#[$m:meta])* extern $abi:literal fn $sym:ident ( $( ( $name:ident $typ:ty ) )* ) $return_type:tt
        $( ( $($e:tt)* ))*
    ) => (
        $(#[$m]);*
        extern $abi fn $sym( $($name : $typ),* ) -> $return_type {
            $( $crate::lisp!( $($e)* ) );*
        }
    );
    // extern fn & void
    ( $(#[$m:meta])* extern $abi:literal fn $sym:ident ( $( ( $name:ident $typ:ty ) )* )
        $( ( $($e:tt)* ))*
    ) => (
        $(#[$m]);*
        extern $abi fn $sym( $($name : $typ),* ) {
            $( $crate::lisp!( $($e)* ) );*
        }
    );
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

    // ── Assertions ───────────────────────────────────────────
    (assert eq $e1:tt $e2:tt) => ( assert_eq!($crate::lisp_arg!($e1), $crate::lisp_arg!($e2)); );
    (debug assert eq $e1:tt $e2:tt) => ( debug_assert_eq!($crate::lisp_arg!($e1), $crate::lisp_arg!($e2)); );
    (debug assert $e1:tt $e2:tt) => ( debug_assert!($crate::lisp_arg!($e1), $crate::lisp_arg!($e2)); );
    (assert $e1:tt $e2:tt) => ( assert!($e1, $e2); );

    // ── Output ───────────────────────────────────────────────
    (print $( $e:tt )+) => ( print!( $($e),+ ) );
    (println $( $e:tt )+) => ( println!( $($e),+ ) );
    (format $( $e:tt )+) => ( format!( $($e),+ ) );
    (panic $($arg:tt)+ ) => ( panic!( $($arg)+ ); );

    // ── Logical ──────────────────────────────────────────────
    (and $a:tt $b:tt $($rest:tt)+) => ($crate::lisp!(and {$crate::lisp_arg!($a) && $crate::lisp_arg!($b)} $($rest)+));
    (and $x:tt $y:tt) => ($crate::lisp_arg!($x) && $crate::lisp_arg!($y));
    (or $a:tt $b:tt $($rest:tt)+) => ($crate::lisp!(or {$crate::lisp_arg!($a) || $crate::lisp_arg!($b)} $($rest)+));
    (or $x:tt $y:tt) => ($crate::lisp_arg!($x) || $crate::lisp_arg!($y));
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
    (& $a:tt $b:tt) => ($crate::lisp_arg!($a) & $crate::lisp_arg!($b));
    (| $a:tt $b:tt) => ($crate::lisp_arg!($a) | $crate::lisp_arg!($b));
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
    (range $a:tt $b:tt) => ($crate::lisp_arg!($a)..$crate::lisp_arg!($b));
    (range= $a:tt $b:tt) => ($crate::lisp_arg!($a)..=$crate::lisp_arg!($b));
    (.. $a:tt $b:tt) => ($crate::lisp_arg!($a)..$crate::lisp_arg!($b));
    (..= $a:tt $b:tt) => ($crate::lisp_arg!($a)..=$crate::lisp_arg!($b));
    (.. $a:tt) => ($crate::lisp_arg!($a)..);
    (..) => (..);

    // ── Index ────────────────────────────────────────────────
    (index $coll:tt $key:tt) => ($crate::lisp_arg!($coll)[$crate::lisp_arg!($key)]);

    // ── Field ────────────────────────────────────────────────
    (field $obj:tt $name:ident) => ($crate::lisp_arg!($obj).$name);

    // . field access (chained)
    (. $obj:tt $( $name:ident )+) => ($crate::lisp_arg!($obj) $(.$name)+);

    // ── Construction ─────────────────────────────────────────
    (new $name:ident $( ($field:ident $val:tt) )* ) => ( $name { $( $field: $crate::lisp_arg!($val) ),* } );
    // struct-lit (struct construction with hyphenated keyword)
    (struct - lit $name:ident $( ($field:ident $val:tt) )* ) => ( $name { $( $field: $crate::lisp_arg!($val) ),* } );
    (box $e:tt) => (Box::new($crate::lisp_arg!($e)));

    // ── Len ──────────────────────────────────────────────────
    (len $e:tt) => ($crate::lisp_arg!($e).len());

    // ── Collections ──────────────────────────────────────────
    (tuple $single:tt) => (($crate::lisp_arg!($single),));
    (tuple $($e:tt)* ) => ( ($($crate::lisp_arg!($e)),*) );
    (vec $($e:tt)* ) => ( vec![$($crate::lisp_arg!($e)),*] );
    (array - repeat $val:tt $count:tt) => ( [$crate::lisp_arg!($val); $crate::lisp_arg!($count)] );
    (array $($e:tt)* ) => ( [$($crate::lisp_arg!($e)),*] );

    // ── Val ──────────────────────────────────────────────────
    (val $e:tt) => ($crate::lisp_arg!($e));

    // ── Rust escape ──────────────────────────────────────────
    (rust { $($t:tt)* }) => ({ $($t)* });
    (rust $( $st:stmt )* ) => ( $($st);* );

    // ── Macro invocation ─────────────────────────────────────
    (macro ! $name:ident $(:: $name2:ident)* $($args:tt)*) => ($name $(:: $name2)* ! ($($crate::lisp_arg!($args)),*));

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
    ($e:expr) => ($e);
    ( $($e:tt)* ) => ($crate::lisp!( $($e)* ));
}
