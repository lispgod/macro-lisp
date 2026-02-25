#![allow(unused_variables)]
#![allow(unused_mut)]
use macro_lisp::lisp;

// P12: Multi-expression bodies

#[test]
fn while_multi_body() {
    lisp!(let mut x 0);
    lisp!(let mut sum 0);
    lisp!(while (< x 5)
        (+= sum x)
        (+= x 1));
    assert_eq!(sum, 10);
}

#[test]
fn loop_multi_body() {
    let r: i32 = lisp!(block
        (let mut i 0)
        (let mut sum 0)
        (loop
            (if (== i 5) (break sum))
            (+= sum i)
            (+= i 1)));
    assert_eq!(r, 10);
}

#[test]
fn fn_multi_body() {
    lisp!(fn compute ((x i32)) i32
        (let a (* x 2))
        (+ a 1)
    );
    assert_eq!(compute(5), 11);
}

#[test]
fn closure_multi_body() {
    lisp!(let f (fn ((x i32))
        (let a (* x 2))
        (+ a 1)));
    assert_eq!(f(5), 11);
}

#[test]
fn for_multi_body() {
    lisp!(let mut evens 0);
    lisp!(let mut odds 0);
    lisp!(for i in (.. 0 10)
        (if (== (% i 2) 0)
            (+= evens 1)
            (+= odds 1)));
    assert_eq!(evens, 5);
    assert_eq!(odds, 5);
}
