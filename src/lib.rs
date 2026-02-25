//! `macro_lisp` — backward-compatible re-export of the `lisp` crate.
//!
//! All macros are now defined in the `lisp` crate. This crate re-exports
//! them so that existing code using `macro_lisp::lisp!` continues to work.

pub use lisp::lisp;
pub use lisp::lisp_arg;
pub use lisp::lisp_match_arg;
pub use lisp::lisp_assign;
pub use lisp::lisp_impl;
pub use lisp::lisp_trait;
pub use lisp::lisp_enum;
pub use lisp::lisp_struct;
pub use lisp::lisp_fn;
