//! `macro_lisp` — backward-compatible re-export of the `lisp` crate.
//!
//! All macros are now defined in the `lisp` crate. This crate re-exports
//! them so that existing code using `macro_lisp::lisp!` continues to work.
//!
//! # Quick Start
//!
//! ```rust
//! use macro_lisp::lisp;
//!
//! // Arithmetic
//! let sum = lisp!(+ 1 2 3);
//! assert_eq!(sum, 6);
//!
//! // Variables
//! lisp!(let x 42);
//! assert_eq!(x, 42);
//!
//! // Collections
//! lisp!(let v (vec 1 2 3));
//! assert_eq!(v, vec![1, 2, 3]);
//! ```
//!
//! # Functions
//!
//! ```rust
//! use macro_lisp::lisp;
//!
//! lisp!(fn add ((a i32) (b i32)) i32
//!     (+ a b));
//!
//! assert_eq!(add(3, 4), 7);
//! ```
//!
//! # Control Flow
//!
//! ```rust
//! use macro_lisp::lisp;
//!
//! let x = 10;
//! let result = lisp!(if (> x 5) "big" "small");
//! assert_eq!(result, "big");
//! ```
//!
//! # Structs
//!
//! ```rust
//! use macro_lisp::lisp;
//!
//! lisp!(struct Point ((x i32) (y i32)));
//! lisp!(let p (new Point (x 10) (y 20)));
//! assert_eq!(p.x, 10);
//! assert_eq!(p.y, 20);
//! ```
//!
//! # Closures
//!
//! ```rust
//! use macro_lisp::lisp;
//!
//! lisp!(let double (fn ((x i32)) (* x 2)));
//! assert_eq!(double(5), 10);
//!
//! // Zero-parameter closure
//! lisp!(let greet (fn () (format! "hello")));
//! assert_eq!(greet(), "hello");
//! ```
//!
//! # Match
//!
//! ```rust
//! use macro_lisp::lisp;
//!
//! let x = 2;
//! let name = lisp!(match x
//!     (1 => "one")
//!     (2 => "two")
//!     (_ => "other"));
//! assert_eq!(name, "two");
//! ```

pub use lisp::lisp;
pub use lisp::lisp_assign;
pub use lisp::lisp_enum;
pub use lisp::lisp_eval;
pub use lisp::lisp_fn;
pub use lisp::lisp_impl;
pub use lisp::lisp_let;
pub use lisp::lisp_struct;
pub use lisp::lisp_trait;
