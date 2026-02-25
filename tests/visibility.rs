#![allow(dead_code)]
use macro_lisp::lisp;

// P14: Visibility beyond pub

lisp!(pub(crate) fn crate_internal ((x i32)) i32
    (+ x 1));

#[test]
fn pub_crate_fn() {
    assert_eq!(crate_internal(5), 6);
}

lisp!(pub(crate) const CRATE_CONST i32 = 42);

#[test]
fn pub_crate_const() {
    assert_eq!(CRATE_CONST, 42);
}

lisp!(pub(crate) type CrateInt = i32);

#[test]
fn pub_crate_type_alias() {
    let x: CrateInt = 10;
    assert_eq!(x, 10);
}
