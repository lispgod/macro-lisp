#![allow(dead_code)]
use macro_lisp::lisp;

// P14: Visibility beyond pub — all item forms support $vis:vis uniformly.

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

// Unified $vis:vis support: struct, enum, trait, static, mod

lisp!(pub(crate) struct VisStruct ((x i32)));

#[test]
fn pub_crate_struct() {
    let s = VisStruct { x: 5 };
    assert_eq!(s.x, 5);
}

lisp!(pub(crate) static VIS_STATIC i32 = 99);

#[test]
fn pub_crate_static() {
    assert_eq!(VIS_STATIC, 99);
}

lisp!(pub(crate) const fn crate_const_fn ((x i32)) i32
    (+ x 10));

#[test]
fn pub_crate_const_fn() {
    assert_eq!(crate_const_fn(5), 15);
}
