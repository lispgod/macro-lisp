# macro-lisp

Lisp-like DSL for Rust language.

[![CI](https://github.com/lispgod/macro-lisp/actions/workflows/ci.yml/badge.svg)](https://github.com/lispgod/macro-lisp/actions/workflows/ci.yml)
[![MIT License](http://img.shields.io/badge/license-MIT-blue.svg?style=flat)](LICENSE)

Write Rust code using S-expression (Lisp-like) syntax via the `lisp!` macro. Everything compiles down to normal Rust — zero runtime overhead.

## Installation

Add to your `Cargo.toml`:

```toml
[dependencies]
macro_lisp = "0.2.0"
```

Then import the macro:

```rust
use macro_lisp::lisp;
```

## Quick Start

```rust
use macro_lisp::lisp;

lisp!(fn factorial ((n i32)) i32
  (if (<= n 1)
    1
    (* n (factorial (- n 1)))));

lisp!(fn main () ()
    (let num (factorial 10))
    (println! "10! = {}" num));
```

## Features

### Functions

```rust
// named function with return type
lisp!(fn add ((a i32) (b i32)) i32
    (+ a b));

// void function
lisp!(fn greet ()
    (println! "Hello!"));

// public, async, const, unsafe, extern functions are all supported
lisp!(pub fn double ((x i32)) i32 (* x 2));
```

### Closures

```rust
lisp!(let add (fn ((a i32) (b i32)) (+ a b)));
lisp!(let inc (fn move ((x i32)) (+ x 1)));
```

### Variables & Assignment

```rust
lisp!(let x 42);                     // let x = 42;
lisp!(let mut y 0);                  // let mut y = 0;
lisp!(let (z i32) (+ 1 2));          // let z: i32 = 1 + 2;
lisp!(= y 10);                       // y = 10;
lisp!(+= y 5);                       // y += 5;
```

### Struct Definition & Construction

```rust
lisp!(struct Point ((x i32) (y i32)));

// Construct with `new`:
lisp!(let p (new Point (x 10) (y 20)));
```

### Control Flow

```rust
// if / else
lisp!(if (> x 0) (println! "positive") (println! "non-positive"));

// match
lisp!(match x
    (1 => (println! "one"))
    (2 => (println! "two"))
    (_ => (println! "other")));

// loops
lisp!(for i in (.. 0 10) (println! "{}" i));
lisp!(while (> n 0) (println! "{}" n) (-= n 1));
lisp!(loop (if (== x 0) (break)) (-= x 1));
```

### Macro Invocations

Call any Rust macro directly with `name!` syntax:

```rust
lisp!(println! "Hello, {}!" "world");
lisp!(let s (format! "{} + {} = {}" 1 2 3));
lisp!(assert_eq! 3 (+ 1 2));
```

### Arithmetic & Operators

```rust
lisp!(+ 1 2)        // 1 + 2
lisp!(* 3 4 5)      // 3 * 4 * 5 (variadic)
lisp!(== x y)       // x == y
lisp!(&& a b)       // a && b
lisp!(! flag)       // !flag
```

### Collections

```rust
lisp!(let v (vec 1 2 3));
lisp!(let t (tuple 1 "hello" 3.14));
lisp!(let a (array 10 20 30));
```

### Traits & Impl Blocks

```rust
lisp!(trait Describable
    (fn describe ((&self)) String));

lisp!(impl Describable for Point
    (fn describe ((&self)) String
        (format! "({}, {})" (self.x) (self.y))));
```

### Enums

```rust
lisp!(enum Color
    (Red)
    (Green)
    (Blue)
    (Custom ((r u8) (g u8) (b u8))));
```

### Modules & Imports

```rust
lisp!(use std::collections::HashMap);

lisp!(pub mod utils
    (pub fn helper () ()
        (println! "I'm a helper")));
```

### Comments

Use Rust-style `///` doc comments instead of Lisp-style `;` comments:

```rust
lisp!(
    /// Adds two integers
    fn add ((a i32) (b i32)) i32
        (+ a b));

// Regular Rust comments between lisp! invocations
lisp!(let x (add 1 2));
```

## Full Example: FizzBuzz

```rust
use macro_lisp::lisp;

lisp!(fn main () ()
    (for num in (..= 1 100)
        (if (== (% num 15) 0)
            (println! "FizzBuzz")
            (if (== (% num 3) 0)
                (println! "Fizz")
                (if (== (% num 5) 0)
                    (println! "Buzz")
                    (println! "{}" num))))));
```

## Syntax Reference

See the full [syntax reference](lisp/src/lib.rs) in the source doc comments.

## Running `.lisp` Files

You can write S-expression programs in standalone `.lisp` files and run them directly:

```lisp
;; factorial.lisp
(fn factorial ((n i32)) i32
  (if (<= n 1)
    1
    (* n (factorial (- n 1)))))

(fn main () ()
    (println! "10! = {}" (factorial 10)))
```

```bash
cargo run -p macro-lisp-cli -- run factorial.lisp      # compile and run
cargo run -p macro-lisp-cli -- check factorial.lisp     # compile only, report errors
cargo run -p macro-lisp-cli -- expand factorial.lisp    # show generated Rust code
```

See `scripts/` for more example programs and `ROADMAP.md` for the project roadmap.

## Interactive REPL

Start an interactive S-expression evaluation session:

```bash
cargo run -p macro-lisp-repl
```

```
λ> (+ 1 2)
3
λ> (fn double ((x i32)) i32 (* x 2))
λ> (double 21)
42
λ> :load scripts/factorial.lisp
Loaded 2 expression(s) from scripts/factorial.lisp
λ> (factorial 5)
120
λ> :expand (+ 1 2)
λ> :reset
λ> :quit
```

Type `:help` in the REPL for all available commands.

## License

[MIT](LICENSE)

