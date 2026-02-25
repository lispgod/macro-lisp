#![allow(unused_variables)]
#![allow(unused_mut)]
#![allow(unused_assignments)]

use macro_lisp::lisp;

#[test]
fn let_immutable() {
    lisp!(let x 5);
    assert_eq!(x, 5);
}

#[test]
fn let_mutable() {
    lisp!(let mut x 5);
    lisp!(= x 10);
    assert_eq!(x, 10);
}

#[test]
fn let_typed() {
    lisp!(let (x i32) 5);
    assert_eq!(x, 5i32);
}

#[test]
fn let_mut_typed() {
    lisp!(let mut (x i32) 5);
    lisp!(= x 10);
    assert_eq!(x, 10i32);
}

#[test]
fn let_with_expression() {
    lisp!(let x (+ 2 3));
    assert_eq!(x, 5);
}

#[test]
fn let_mut_with_expression() {
    lisp!(let mut x (+ 2 3));
    assert_eq!(x, 5);
    lisp!(= x (+ 10 20));
    assert_eq!(x, 30);
}

#[test]
fn let_typed_with_expression() {
    lisp!(let (x i64) (+ 10 20));
    assert_eq!(x, 30i64);
}

#[test]
fn let_mut_typed_with_expression() {
    lisp!(let mut (y i32) (* 3 4));
    assert_eq!(y, 12i32);
    lisp!(= y (* 5 6));
    assert_eq!(y, 30i32);
}

#[test]
fn scoped_let() {
    let result = lisp!(let ((x 1) (y 2)) (+ x y));
    assert_eq!(result, 3);
}

#[test]
fn scoped_let_mutation() {
    lisp!(block
        (let x 100)
        (let ((a 1) (b 2))
            (+= a 10)
            (-= b 1)
            (assert_eq! 11 a)
            (assert_eq! 1 b))
        (assert_eq! x 100)
    );
}

#[test]
fn set_with_expression() {
    lisp!(let mut x 0);
    lisp!(= x (+ 3 4));
    assert_eq!(x, 7);
}

#[test]
fn compound_assignment_add() {
    lisp!(let mut x 10);
    lisp!(+= x 5);
    assert_eq!(x, 15);
}

#[test]
fn compound_assignment_sub() {
    lisp!(let mut x 10);
    lisp!(-= x 3);
    assert_eq!(x, 7);
}

#[test]
fn compound_assignment_mul() {
    lisp!(let mut x 5);
    lisp!(*= x 3);
    assert_eq!(x, 15);
}

#[test]
fn compound_assignment_div() {
    lisp!(let mut x 20);
    lisp!(/= x 4);
    assert_eq!(x, 5);
}

#[test]
fn compound_assignment_rem() {
    lisp!(let mut x 17);
    lisp!(%= x 5);
    assert_eq!(x, 2);
}

#[test]
fn compound_assignment_with_expr() {
    lisp!(let mut x 10);
    lisp!(+= x (+ 2 3));
    assert_eq!(x, 15);
}
