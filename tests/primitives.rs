#![allow(unused_variables)]

use macro_lisp::lisp;

#[test]
fn integer_literal() {
    let x = lisp!(42);
    assert_eq!(x, 42);
}

#[test]
fn float_literal() {
    let x = lisp!(3.14);
    assert!((x - 3.14_f64).abs() < 1e-10);
}

#[test]
fn bool_true() {
    let x: bool = lisp!(true);
    assert!(x);
}

#[test]
fn bool_false() {
    let x: bool = lisp!(false);
    assert!(!x);
}

#[test]
fn char_literal() {
    let c = lisp!('c');
    assert_eq!(c, 'c');
}

#[test]
fn string_literal() {
    let s = lisp!("hello");
    assert_eq!(s, "hello");
}

#[test]
fn byte_string_literal() {
    let b = lisp!(b"hello");
    assert_eq!(b, b"hello");
}

#[test]
fn bare_ident_passthrough_in_expr() {
    // Inside expressions, bare idents pass through via lisp_arg!
    let x = 42;
    let y = lisp!(+ x 0);
    assert_eq!(y, 42);
}

#[test]
fn bare_ident_passthrough_via_val() {
    // The `val` form extracts a bare value
    let x = 42;
    let y = lisp!(val x);
    assert_eq!(y, 42);
}

#[test]
fn tuple_unit() {
    let _t: () = lisp!(tuple);
}
