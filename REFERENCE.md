# macro-lisp Language Reference

Complete reference for every operative in the `lisp!` macro. Each entry shows the S-expression syntax template and one concrete input→output example.

---

## Atoms

| S-expression | Rust output |
|---|---|
| `true` | `true` |
| `false` | `false` |
| `42` | `42` |
| `3.14` | `3.14` |
| `"hello"` | `"hello"` |
| `'c'` | `'c'` |
| `self.x.y` | `self.x.y` |
| `(val x)` | `x` |

```
lisp!(true)        →  true
lisp!(42)          →  42
lisp!(val x)       →  x
```

---

## Bindings

### `let` — Immutable Binding

| Syntax | Output |
|---|---|
| `(let var val)` | `let var = val;` |
| `(let var (expr))` | `let var = expr;` |
| `(let (var Type) val)` | `let var: Type = val;` |
| `(let (var Type) (expr))` | `let var: Type = expr;` |

```
lisp!(let x 42)           →  let x = 42;
lisp!(let (z i32) (+ 1 2))  →  let z: i32 = 1 + 2;
```

### `let mut` — Mutable Binding

| Syntax | Output |
|---|---|
| `(let mut var val)` | `let mut var = val;` |
| `(let mut var (expr))` | `let mut var = expr;` |
| `(let mut (var Type) val)` | `let mut var: Type = val;` |
| `(let mut (var Type) (expr))` | `let mut var: Type = expr;` |

```
lisp!(let mut y 0)              →  let mut y = 0;
lisp!(let mut (w i32) (+ 1 2))  →  let mut w: i32 = 1 + 2;
```

### Scoped Mutable Bindings

```
(let ((var1 init1) (var2 init2) ...) (body)...)
```

Creates a block with `let mut` bindings.

```
lisp!(let ((a 10) (b 20))
    (+ a b))
→  { let mut a = 10; let mut b = 20; a + b }
```

### Pattern Destructuring

```
(let pattern value)
```

Falls through to proc macro for complex patterns.

```
lisp!(let (a, b) pair)              →  let (a, b) = pair;
lisp!(let Point { x, y } p)        →  let Point { x, y } = p;
```

### `=` — Assignment

| Syntax | Output |
|---|---|
| `(= var val)` | `var = val;` |
| `(= var (expr))` | `var = expr;` |
| `(= obj.field val)` | `obj.field = val;` |

```
lisp!(= x 10)       →  x = 10;
lisp!(= x (+ 2 3))  →  x = 2 + 3;
```

### Compound Assignment

| Syntax | Output |
|---|---|
| `(+= var e)` | `var += e;` |
| `(-= var e)` | `var -= e;` |
| `(*= var e)` | `var *= e;` |
| `(/= var e)` | `var /= e;` |
| `(%= var e)` | `var %= e;` |
| `(&= var e)` | `var &= e;` |
| `(\|= var e)` | `var \|= e;` |
| `(^= var e)` | `var ^= e;` |
| `(<<= var e)` | `var <<= e;` |
| `(>>= var e)` | `var >>= e;` |

All compound assignment operators also work on complex LHS expressions (field access, index, etc.) via the proc macro fallback.

```
lisp!(+= x 5)           →  x += 5;
lisp!(<<= bits 3)       →  bits <<= 3;
lisp!(+= self.count 1)  →  self.count += 1;
```

### `const` — Constant

| Syntax | Output |
|---|---|
| `(const NAME Type = val)` | `const NAME: Type = val;` |
| `(const NAME Type val)` | `const NAME: Type = val;` |
| `(pub const NAME Type = val)` | `pub const NAME: Type = val;` |

```
lisp!(const PI f64 = 3.14159)  →  const PI: f64 = 3.14159;
lisp!(const MAX i32 100)       →  const MAX: i32 = 100;
```

### `static` — Static Variable

| Syntax | Output |
|---|---|
| `(static NAME Type = val)` | `static NAME: Type = val;` |
| `(static NAME Type val)` | `static NAME: Type = val;` |
| `(static mut NAME Type = val)` | `static mut NAME: Type = val;` |
| `(static mut NAME Type val)` | `static mut NAME: Type = val;` |

```
lisp!(static COUNT i32 = 0)        →  static COUNT: i32 = 0;
lisp!(static mut TOTAL i32 = 0)    →  static mut TOTAL: i32 = 0;
```

---

## Operators

### Arithmetic (variadic)

| Syntax | Output |
|---|---|
| `(+ a b ...)` | `a + b + ...` |
| `(- a b ...)` | `a - b - ...` |
| `(* a b ...)` | `a * b * ...` |
| `(/ a b ...)` | `a / b / ...` |
| `(% a b)` | `a % b` |
| `(neg x)` | `-x` |

```
lisp!(+ 1 2 3 4 5)  →  1 + 2 + 3 + 4 + 5
lisp!(* 2 3 4)       →  2 * 3 * 4
lisp!(neg x)         →  -x
```

### Comparison

| Syntax | Output |
|---|---|
| `(== x y)` | `x == y` |
| `(!= x y)` | `x != y` |
| `(< x y)` | `x < y` |
| `(> x y)` | `x > y` |
| `(<= x y)` | `x <= y` |
| `(>= x y)` | `x >= y` |

```
lisp!(== x 5)   →  x == 5
lisp!(<= a b)   →  a <= b
```

### Logical

| Syntax | Output |
|---|---|
| `(&& a b)` | `a && b` |
| `(\|\| a b)` | `a \|\| b` |
| `(! x)` | `!x` |

```
lisp!(&& true false)  →  true && false
lisp!(! flag)         →  !flag
```

### Bitwise

| Syntax | Output |
|---|---|
| `(& a b)` | `a & b` |
| `(\| a b)` | `a \| b` |
| `(^ a b)` | `a ^ b` |
| `(<< a b)` | `a << b` |
| `(>> a b)` | `a >> b` |

```
lisp!(& 0xFF 0x0F)  →  0xFF & 0x0F
lisp!(<< 1 4)       →  1 << 4
```

---

## Control Flow

### `if` / `if else`

| Syntax | Output |
|---|---|
| `(if cond then)` | `if cond { then }` |
| `(if cond then else)` | `if cond { then } else { else }` |
| `(if (cond) then else)` | `if cond { then } else { else }` |

```
lisp!(if (> x 0) "positive" "non-positive")
→  if x > 0 { "positive" } else { "non-positive" }
```

### `if let`

| Syntax | Output |
|---|---|
| `(if let (Pat = e) then)` | `if let Pat = e { then }` |
| `(if let (Pat = e) then else)` | `if let Pat = e { then } else { else }` |

```
lisp!(if let (Some(v) = opt) v 0)
→  if let Some(v) = opt { v } else { 0 }
```

### `match`

```
(match expr
    (pattern => (body))
    (pattern => (body))
    ...)
```

Supports pattern alternatives with `|`.

```
lisp!(match x
    (1 => "one")
    (2 | 3 => "few")
    (_ => "many"))
→  match x { 1 => "one", 2 | 3 => "few", _ => "many" }
```

### `while`

| Syntax | Output |
|---|---|
| `(while cond (body)...)` | `while cond { body }` |
| `('label while cond (body)...)` | `'label: while cond { body }` |

```
lisp!(while (< n 10) (+= n 1))  →  while n < 10 { n += 1; }
```

### `while let`

```
(while let (Pat = expr) (body)...)
```

```
lisp!(while let (Some(v) = (stack.pop)) (+= sum v))
→  while let Some(v) = stack.pop() { sum += v; }
```

### `loop`

| Syntax | Output |
|---|---|
| `(loop (body)...)` | `loop { body }` |
| `('label loop (body)...)` | `'label: loop { body }` |

```
lisp!(loop (if (== x 0) (break)) (-= x 1))
→  loop { if x == 0 { break; }; x -= 1; }
```

### `for`

| Syntax | Output |
|---|---|
| `(for var in iter (body)...)` | `for var in iter { body }` |
| `('label for var in iter (body)...)` | `'label: for var in iter { body }` |

```
lisp!(for i in (.. 0 10) (println! "{}" i))
→  for i in 0..10 { println!("{}", i) }
```

### `block`

| Syntax | Output |
|---|---|
| `(block (stmt)...)` | `{ stmt; ... }` |
| `(block 'label (stmt)...)` | `'label: { stmt; ... }` |

```
lisp!(block (let a 10) (let b 20) (+ a b))
→  { let a = 10; let b = 20; a + b }
```

### `break`

| Syntax | Output |
|---|---|
| `(break)` | `break;` |
| `(break expr)` | `break expr;` |
| `(break 'label)` | `break 'label;` |
| `(break 'label expr)` | `break 'label expr;` |

```
lisp!(break 'outer 42)  →  break 'outer 42;
```

### `continue`

| Syntax | Output |
|---|---|
| `(continue)` | `continue;` |
| `(continue 'label)` | `continue 'label;` |

```
lisp!(continue 'outer)  →  continue 'outer;
```

### `return`

| Syntax | Output |
|---|---|
| `(return)` | `return` |
| `(return val)` | `return val` |
| `(return (expr))` | `return expr` |

```
lisp!(return (+ x 1))  →  return x + 1
```

---

## Functions

### Named Functions

```
([vis] [qualifiers] fn name [<generics>] ((params)) [ReturnType] [where (clause)] (body)...)
```

Qualifiers: `pub`, `pub(crate)`, `pub(super)`, `const`, `unsafe`, `async`, `extern "C"`.

```
lisp!(fn add ((a i32) (b i32)) i32
    (+ a b))
→  fn add(a: i32, b: i32) -> i32 { a + b }

lisp!(pub fn double ((x i32)) i32 (* x 2))
→  pub fn double(x: i32) -> i32 { x * 2 }

lisp!(const fn max ((a i32) (b i32)) i32
    (if (> a b) a b))
→  const fn max(a: i32, b: i32) -> i32 { if a > b { a } else { b } }

lisp!(unsafe fn danger ((p *const i32)) i32
    (deref p))
→  unsafe fn danger(p: *const i32) -> i32 { *p }

lisp!(extern "C" fn c_add ((a i32) (b i32)) i32 (+ a b))
→  extern "C" fn c_add(a: i32, b: i32) -> i32 { a + b }
```

### Functions with Generics

```
lisp!(fn generic_add <T: core::ops::Add<Output = T>> ((a T) (b T)) T
    (+ a b))
→  fn generic_add<T: core::ops::Add<Output = T>>(a: T, b: T) -> T { a + b }
```

### Functions with Where Clauses

```
lisp!(fn debug_val <T> ((val &T)) String where (T: core::fmt::Debug)
    (format! "{:?}" val))
→  fn debug_val<T>(val: &T) -> String where T: core::fmt::Debug { format!("{:?}", val) }
```

### Void Functions (no return type)

```
lisp!(fn greet () (println! "Hello!"))
→  fn greet() { println!("Hello!") }
```

### Self Parameters

| Syntax | Output |
|---|---|
| `((&self))` | `&self` |
| `((&mut self))` | `&mut self` |
| `((self Self))` | `self: Self` (owned) |
| `((self))` | `self` |
| `((mut self))` | `mut self` |

### Function Declarations (no body, in traits)

```
lisp!(trait Foo
    (fn bar ((&self)) i32))
→  trait Foo { fn bar(&self) -> i32; }
```

---

## Closures

| Syntax | Output |
|---|---|
| `(fn ((x Type)...) (body)...)` | `\|x: Type, ...\| { body }` |
| `(fn move ((x Type)...) (body)...)` | `move \|x: Type, ...\| { body }` |
| `(fn ((x)...) (body)...)` | `\|x, ...\| { body }` |
| `(fn move ((x)...) (body)...)` | `move \|x, ...\| { body }` |

```
lisp!(fn ((a i32) (b i32)) (+ a b))
→  |a: i32, b: i32| { a + b }

lisp!(fn move ((x)) (* x 2))
→  move |x| { x * 2 }
```

---

## Calls

### Function Call

```
(func arg1 arg2 ...)  →  func(arg1, arg2, ...)
```

```
lisp!(add 3 4)  →  add(3, 4)
```

### Method Call

```
(obj.method arg1 ...)  →  obj.method(arg1, ...)
```

```
lisp!(v.push 42)  →  v.push(42)
```

### Path-Qualified Call

```
(path::func arg1 ...)  →  path::func(arg1, ...)
```

```
lisp!(Point::new 1 2)  →  Point::new(1, 2)
```

---

## Macro Invocation

### Shorthand (preferred)

```
(name! args...)  →  name!(args...)
```

```
lisp!(println! "x = {}" x)     →  println!("x = {}", x)
lisp!(assert_eq! a b)          →  assert_eq!(a, b)
lisp!(format! "{} {}" a b)     →  format!("{} {}", a, b)
```

### Path-Qualified Macro

```
(path::name! args...)  →  path::name!(args...)
```

### `macro!` Keyword Form

```
(macro ! name args...)  →  name!(args...)
```

```
lisp!(macro ! assert_eq x 5)  →  assert_eq!(x, 5)
```

---

## Type Definitions

### `struct`

| Syntax | Output |
|---|---|
| `(struct Name ((f1 T1) (f2 T2)))` | `struct Name { f1: T1, f2: T2 }` |
| `(struct Name (T1 T2))` | `struct Name(T1, T2);` |
| `(struct Name)` | `struct Name;` |
| `(pub struct Name ((f T)))` | `pub struct Name { f: T }` |
| `(struct Name (pub (f T)))` | `struct Name { pub f: T }` |
| `(#[derive(Debug)] struct Name ...)` | `#[derive(Debug)] struct Name ...` |

```
lisp!(struct Point ((x i32) (y i32)))
→  struct Point { x: i32, y: i32 }

lisp!(struct Pair (i32 i32))
→  struct Pair(i32, i32);

lisp!(struct Marker)
→  struct Marker;
```

### Structs with Generics

```
lisp!(struct Container<T: Clone> ((val T)))
→  struct Container<T: Clone> { val: T }

lisp!(struct StrRef<'a> ((s &'a str)))
→  struct StrRef<'a> { s: &'a str }
```

### Structs with Where Clause

```
lisp!(struct Holder<T> where (T: Clone) ((inner T)))
→  struct Holder<T> where T: Clone { inner: T }
```

### `enum`

```
(enum Name
    (Variant)                       → unit variant
    (Variant Type1 Type2)           → tuple variant
    (Variant ((f1 T1) (f2 T2))))   → struct variant
```

```
lisp!(enum Shape
    (Circle f64)
    (Rect ((w f64) (h f64)))
    (Unit))
→  enum Shape { Circle(f64), Rect { w: f64, h: f64 }, Unit }
```

### Generic Enums

```
lisp!(enum Maybe<T>
    (Just T)
    (Nothing))
→  enum Maybe<T> { Just(T), Nothing }
```

### `type` — Type Alias

```
(type Name = Target)
```

```
lisp!(type IntPair = (i32, i32))
→  type IntPair = (i32, i32);
```

---

## Traits and Impls

### `trait`

```
([vis] trait Name [<generics>] [: Supertrait] [where (clause)]
    (fn method (params) [RetType] [body])
    (type AssocType)
    ...)
```

```
lisp!(trait Describable
    (fn describe ((&self)) String))
→  trait Describable { fn describe(&self) -> String; }

lisp!(pub trait Printable : core::fmt::Display
    (fn print ((&self))
        (println! "{}" self)))
→  pub trait Printable: core::fmt::Display { fn print(&self) { println!("{}", self) } }
```

### `impl` — Inherent

```
(impl [<generics>] Type [where (clause)]
    (fn method ...)
    ...)
```

```
lisp!(impl Point
    (fn new ((x i32) (y i32)) Point
        (new Point (x x) (y y))))
→  impl Point { fn new(x: i32, y: i32) -> Point { Point { x: x, y: y } } }
```

### `impl` — Trait

```
(impl [<generics>] Trait for Type [where (clause)]
    (type AssocType = Concrete)
    (fn method ...)
    ...)
```

```
lisp!(impl core::fmt::Display for Point
    (fn fmt ((self &Self) (f &mut core::fmt::Formatter)) core::fmt::Result
        (write! f "({}, {})" (self.x) (self.y))))
→  impl core::fmt::Display for Point {
       fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
           write!(f, "({}, {})", self.x, self.y)
       }
   }
```

### Generic Impl

```
lisp!(impl<T: Clone> Container<T>
    (fn unwrap ((&self)) T
        (rust { self.val.clone() })))
→  impl<T: Clone> Container<T> { fn unwrap(&self) -> T { self.val.clone() } }
```

### Associated Types in Impl

```
lisp!(impl core::ops::Add for Point
    (type Output = Point)
    (fn add ((self Self) (rhs Self)) Point ...))
→  impl core::ops::Add for Point { type Output = Point; fn add(self, rhs: Self) -> Point { ... } }
```

---

## Construction

### `new` — Struct Literal

| Syntax | Output |
|---|---|
| `(new Name (f1 v1) (f2 v2))` | `Name { f1: v1, f2: v2 }` |
| `(new Name f1 f2)` | `Name { f1, f2 }` (shorthand) |
| `(new Name (f1 v1) (.. base))` | `Name { f1: v1, ..base }` (spread) |

```
lisp!(new Point (x 10) (y 20))    →  Point { x: 10, y: 20 }
lisp!(new Point x y)               →  Point { x, y }
lisp!(new Point (x 99) (.. base))  →  Point { x: 99, ..base }
```

### `tuple`

| Syntax | Output |
|---|---|
| `(tuple)` | `()` |
| `(tuple a)` | `(a,)` |
| `(tuple a b c)` | `(a, b, c)` |

```
lisp!(tuple 1 2 3)  →  (1, 2, 3)
```

### `array`

```
(array e1 e2 ...)  →  [e1, e2, ...]
```

```
lisp!(array 10 20 30)  →  [10, 20, 30]
```

For array-repeat syntax, use raw Rust bracket expressions directly:

```
[0; 5]   →  [0, 0, 0, 0, 0]
[42; 3]  →  [42, 42, 42]
```

### `vec`

```
(vec e1 e2 ...)  →  vec![e1, e2, ...]
```

```
lisp!(vec 1 2 3)  →  vec![1, 2, 3]
```

---

## References

| Syntax | Output |
|---|---|
| `(ref x)` | `&x` |
| `(ref mut x)` | `&mut x` |
| `(deref x)` | `*x` |
| `(as x Type)` | `x as Type` |
| `(? x)` | `x?` |
| `(await e)` | `e.await` |

```
lisp!(ref x)       →  &x
lisp!(ref mut x)   →  &mut x
lisp!(deref ptr)   →  *ptr
lisp!(as x u8)     →  x as u8
lisp!(? result)    →  result?
lisp!(await fut)   →  fut.await
```

---

## Access

| Syntax | Output |
|---|---|
| `(index coll key)` | `coll[key]` |
| `(. obj field)` | `obj.field` |
| `(. obj a b c)` | `obj.a.b.c` |
| `(.. a b)` | `a..b` |
| `(..= a b)` | `a..=b` |
| `(.. a)` | `a..` |
| `(..)` | `..` |

```
lisp!(index v 0)      →  v[0]
lisp!(. point x)      →  point.x
lisp!(. obj inner val) →  obj.inner.val
lisp!(.. 0 10)        →  0..10
lisp!(..= 1 100)      →  1..=100
```

---

## Modules

| Syntax | Output |
|---|---|
| `(mod name (body)...)` | `mod name { body }` |
| `(pub mod name (body)...)` | `pub mod name { body }` |
| `(use path::to::item)` | `use path::to::item;` |
| `(extern crate name)` | `extern crate name;` |

```
lisp!(pub mod utils
    (pub fn helper () () (println! "help")))
→  pub mod utils { pub fn helper() { println!("help") } }

lisp!(use std::collections::HashMap)
→  use std::collections::HashMap;
```

---

## Attributes

Attributes are placed before the visibility keyword:

```
(#[derive(Debug, Clone)] struct Name ...)
(#[derive(Debug)] enum Name ...)
```

```
lisp!(#[derive(Debug, PartialEq)]
 struct Pair ((a i32) (b i32)))
→  #[derive(Debug, PartialEq)] struct Pair { a: i32, b: i32 }
```

---

## Unsafe

| Syntax | Output |
|---|---|
| `(unsafe (body)...)` | `unsafe { body }` |
| `(unsafe fn name ...)` | `unsafe fn name(...) { ... }` |

```
lisp!(unsafe (rust { *ptr }))  →  unsafe { *ptr }
lisp!(unsafe fn read_ptr ((p *const i32)) i32 (deref p))
→  unsafe fn read_ptr(p: *const i32) -> i32 { *p }
```

---

## Escape Hatch

| Syntax | Output |
|---|---|
| `(rust { code })` | `{ code }` |
| `(rust stmt...)` | `stmt; ...` |

Use this for Rust expressions the macro can't express.

```
lisp!(rust { let x = vec![1,2,3]; x.iter().sum::<i32>() })
→  { let x = vec![1,2,3]; x.iter().sum::<i32>() }

lisp!(rust let x: i32 = 42)
→  let x: i32 = 42;
```

---

## Visibility Modifiers

All item definitions accept standard Rust visibility:

| Modifier | Example |
|---|---|
| (none) | `(fn foo ...)` → `fn foo(...)` |
| `pub` | `(pub fn foo ...)` → `pub fn foo(...)` |
| `pub(crate)` | `(pub(crate) fn foo ...)` → `pub(crate) fn foo(...)` |
| `pub(super)` | `(pub(super) fn foo ...)` → `pub(super) fn foo(...)` |

Applies to: `struct`, `enum`, `trait`, `fn`, `const`, `static`, `type`, `mod`.

---

## Comments

Use Rust-style `///` doc comments instead of Lisp-style `;` comments. Place them inside `lisp!()` before item definitions:

```rust
lisp!(
    /// Adds two integers
    fn add ((a i32) (b i32)) i32
        (+ a b));

/// You can also use `#[doc = "..."]` directly
lisp!(#[doc = "A 2D point"] struct Point ((x i32) (y i32)));
```

Between `lisp!()` invocations, use regular Rust `//` comments:

```rust
// Bind a variable
lisp!(let x 42);

// Use it in an expression
lisp!(let y (+ x 1));
```

---

## Special Forms

| Form | Description |
|---|---|
| `(panic args...)` | `panic!(args...)` |
| `(self.x.y)` | Field access on `self` |
| `(val x)` | Identity — returns `x` unchanged |
