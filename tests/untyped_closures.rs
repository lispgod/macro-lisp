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
fn closure_zero_params() {
    let f = lisp!(fn () (+ 1 2));
    assert_eq!(f(), 3);
}

#[test]
fn closure_zero_params_move() {
    let x = 42;
    let f = lisp!(fn move () (val x));
    assert_eq!(f(), 42);
}

#[test]
fn closure_zero_params_multi_body() {
    let f = lisp!(fn ()
        (let a 10)
        (let b 20)
        (+ a b));
    assert_eq!(f(), 30);
}

#[test]
fn closure_single_untyped() {
    lisp!(let double (fn ((x)) (* x 2)));
    assert_eq!(double(5i32), 10);
}
