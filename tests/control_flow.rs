#![allow(unused_variables)]
#![allow(unused_mut)]
#![allow(unused_assignments)]
#![allow(unreachable_code)]

use macro_lisp::lisp;

#[test]
fn if_with_paren_cond_and_else() {
    let x = lisp!(if (== 1 1) (+ 1 1) (+ 2 2));
    assert_eq!(x, 2);
}

#[test]
fn if_with_paren_cond_no_else() {
    lisp!(let mut x 0);
    lisp!(if (== 1 1) (= x 42));
    assert_eq!(x, 42);
}

#[test]
fn if_with_simple_cond_and_else() {
    let x = lisp!(if true (+ 1 1) (+ 2 2));
    assert_eq!(x, 2);

    let y = lisp!(if false (+ 1 1) (+ 2 2));
    assert_eq!(y, 4);
}

#[test]
fn if_with_simple_cond_no_else() {
    lisp!(let mut x 0);
    lisp!(if true (= x 42));
    assert_eq!(x, 42);
}

#[test]
fn if_let_with_else() {
    let num = Some(7);
    let x = lisp!(if let (Some(i) = num) (+ i 1) 0);
    assert_eq!(x, 8);
}

#[test]
fn if_let_without_else() {
    lisp!(let mut result 0);
    let num = Some(7);
    lisp!(if let (Some(i) = num) (= result i));
    assert_eq!(result, 7);
}

#[test]
fn when_true() {
    lisp!(let mut x 0);
    lisp!(if true (= x 1));
    assert_eq!(x, 1);
}

#[test]
fn when_false() {
    lisp!(let mut x 0);
    lisp!(if false (= x 1));
    assert_eq!(x, 0);
}

#[test]
fn when_with_cond() {
    lisp!(let mut x 0);
    lisp!(if (== 1 1) (= x 42));
    assert_eq!(x, 42);
}

#[test]
fn unless_true() {
    lisp!(let mut x 0);
    lisp!(if (! true) (= x 1));
    assert_eq!(x, 0);
}

#[test]
fn unless_false() {
    lisp!(let mut x 0);
    lisp!(if (! false) (= x 2));
    assert_eq!(x, 2);
}

#[test]
fn match_basic() {
    let s = "test";
    let x = lisp!(match s
        ("test" => (1))
        (_ => (-1)));
    assert_eq!(x, 1);
}

#[test]
fn match_multiple_patterns() {
    let val = 3;
    let result = lisp!(match val
        (1 | 2 => (10))
        (3 | 4 => (20))
        (_ => (0)));
    assert_eq!(result, 20);
}

#[test]
fn while_loop() {
    lisp!(let mut x 0);
    lisp!(while (< x 5) (+= x 1));
    assert_eq!(x, 5);
}

#[test]
fn while_let_loop() {
    lisp!(block
        (let mut num Some(0))
        (let mut last 0)
        (while let (Some(i) = num)
            (= last i)
            (if (> i 9)
                (= num None)
                (= num Some(i + 1))))
        (assert_eq! 10 last)
    );
}

#[test]
fn for_in_range() {
    lisp!(let mut sum 0);
    lisp!(for i in (.. 0 5) (= sum (+ sum i)));
    assert_eq!(sum, 10);
}

#[test]
fn for_in_vec() {
    let v = vec![1, 2, 3, 4, 5];
    lisp!(let mut sum 0);
    lisp!(for num in v (= sum (+ sum num)));
    assert_eq!(sum, 15);
}

#[test]
fn for_in_inline_vec() {
    lisp!(let ((sum 0))
        (for num in (vec 10 20 30)
            (= sum (+ sum num)))
        (assert_eq! 60 sum)
    );
}

#[test]
fn loop_with_break() {
    lisp!(let mut x 0);
    lisp!(loop (+= x 1) (if (>= x 5) (break)));
    assert_eq!(x, 5);
}

#[test]
fn loop_with_continue() {
    lisp!(block
        (let mut sum 0)
        (for i in (.. 0 10)
            (if (== (% i 2) 0)
                (continue))
            (= sum (+ sum i)))
        (assert_eq! 25 sum)
    );
}

#[test]
fn block_returns_last() {
    let x = lisp!(block
        (let a 3)
        (let b 4)
        (+ a b)
    );
    assert_eq!(x, 7);
}

#[test]
fn nested_if() {
    let x = 15;
    let result = lisp!(if (== (% x 15) 0)
        (format! "FizzBuzz")
        (if (== (% x 3) 0)
            (format! "Fizz")
            (if (== (% x 5) 0)
                (format! "Buzz")
                (format! "{}" x))));
    assert_eq!(result, "FizzBuzz");
}
