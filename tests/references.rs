#![allow(unused_variables)]
#![allow(unused_mut)]
#![allow(unused_unsafe)]

use macro_lisp::lisp;

#[test]
fn ref_immutable() {
    let x = 42;
    let r = lisp!(ref x);
    assert_eq!(*r, 42);
}

#[test]
fn ref_mutable() {
    lisp!(let mut x 42);
    {
        let r = lisp!(ref mut x);
        *r = 99;
    }
    assert_eq!(x, 99);
}

#[test]
fn deref_basic() {
    let x = 42;
    let r = &x;
    let val = lisp!(deref r);
    assert_eq!(val, 42);
}

#[test]
fn as_cast() {
    let x: i32 = 42;
    let y: f64 = lisp!(as x f64);
    assert_eq!(y, 42.0);
}

#[test]
fn as_cast_float_to_int() {
    let x: f64 = 3.14;
    let y: i32 = lisp!(as x i32);
    assert_eq!(y, 3);
}

#[test]
fn index_vec() {
    let v = vec![10, 20, 30, 40, 50];
    let x = lisp!(index v 2);
    assert_eq!(x, 30);
}

#[test]
fn index_slice() {
    let arr = [1, 2, 3, 4, 5];
    let x = lisp!(index arr 0);
    assert_eq!(x, 1);
}

#[test]
fn range_basic() {
    let r = lisp!(range 0 5);
    let v: Vec<i32> = r.collect();
    assert_eq!(v, vec![0, 1, 2, 3, 4]);
}

#[test]
fn range_inclusive() {
    let r = lisp!(range= 1 5);
    let v: Vec<i32> = r.collect();
    assert_eq!(v, vec![1, 2, 3, 4, 5]);
}

#[test]
fn box_basic() {
    let b = lisp!(box 42);
    assert_eq!(*b, 42);
}

#[test]
fn box_with_expr() {
    let b = lisp!(box (+ 1 2));
    assert_eq!(*b, 3);
}
