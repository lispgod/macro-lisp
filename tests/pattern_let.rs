#![allow(unused_variables)]
#![allow(dead_code)]
use macro_lisp::lisp;

// P11: Pattern destructuring in let

#[test]
fn let_tuple_destructure() {
    lisp!(let (a, b) (tuple 1 2));
    assert_eq!(a, 1);
    assert_eq!(b, 2);
}

#[test]
fn let_struct_destructure() {
    struct Point { x: i32, y: i32 }
    let p = Point { x: 10, y: 20 };
    lisp!(let Point { x, y } p);
    assert_eq!(x, 10);
    assert_eq!(y, 20);
}

#[test]
fn let_nested_destructure() {
    lisp!(let (a, (b, c)) (tuple 1 (tuple 2 3)));
    assert_eq!(a, 1);
    assert_eq!(b, 2);
    assert_eq!(c, 3);
}

#[test]
fn let_underscore() {
    lisp!(let _ 42);
}
