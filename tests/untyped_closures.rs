#![allow(unused_variables)]
use macro_lisp::lisp;

// P15: Closures without type annotations

#[test]
fn closure_no_types() {
    lisp!(let add (fn ((x) (y)) (+ x y)));
    assert_eq!(add(3i32, 4i32), 7);
}

#[test]
fn closure_no_params() {
    lisp!(let five (fn () (+ 5 0)));
    assert_eq!(five(), 5);
}

#[test]
fn closure_single_untyped() {
    lisp!(let double (fn ((x)) (* x 2)));
    assert_eq!(double(5i32), 10);
}
