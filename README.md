# macro-lisp

Lisp-like DSL for Rust language.
[![Build Status](https://travis-ci.org/JunSuzukiJapan/macro-lisp.svg?branch=master)](https://travis-ci.org/JunSuzukiJapan/macro-lisp)
[![MIT License](http://img.shields.io/badge/license-MIT-blue.svg?style=flat)](LICENSE)

# Installation

include the following in your Cargo.toml file:

```toml
[dependencies]
macro_lisp="0.2.0"
```

And then import the library using:

```rust
use macro_lisp::lisp;
```

# Examples

## factorial

``` rust
use macro_lisp::lisp;

lisp!(fn factorial ((n i32)) i32
  (if (<= n 1)
    1
    (* n (factorial (- n 1)))));

lisp!(fn main () ()
    (let num (factorial 10))
    (println "10! = {}" num));
```

## wc

``` rust
use macro_lisp::lisp;

lisp!(use std::env);
lisp!(use std::process::exit);

lisp!(fn is_whitespace ((b u8)) bool
    (match b
        (0x20 | 0x09 | 0x85 | 0x0a | 0x0b | 0x0c | 0x0d => (true))
        (_ => (false) ))
);

lisp!(fn main () ()
    (let (args Vec<String>) env::args().collect())
    (if (< (len args) 2)
        (progn
            (println "usage: wc file")
            (exit 0)))

    (defvar char_count 0)
    (defvar word_count 0)
    (defvar line_count 0)
    (defvar in_word false)

    (let path &args[1])
    (with-input-from-file (file path)
        (doiter (byte file.bytes())
            (incf char_count)

            (let b byte.unwrap())
            (if (== b 0x0a)
                (incf line_count))

            (if in_word
                (if (is_whitespace b)
                    (setf in_word false))
                (if (! (is_whitespace b))
                    (progn
                        (setf in_word true)
                        (incf word_count))))))

    (println "{:>10} {:>10} {:>10} {}" line_count word_count char_count path)
);
```

# License

[MIT](LICENSE)

