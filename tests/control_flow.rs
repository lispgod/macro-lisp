#![allow(unused_variables)]
#![allow(unused_mut)]
#![allow(unused_assignments)]
#![allow(unreachable_code)]
#![allow(dead_code)]

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

#[test]
fn cond_form() {
    let classify = |x: i32| -> &'static str {
        lisp!(cond
            ((> x 0) "positive")
            ((< x 0) "negative")
            (else "zero"))
    };
    assert_eq!(classify(5), "positive");
    assert_eq!(classify(-3), "negative");
    assert_eq!(classify(0), "zero");
}

#[test]
fn cond_two_branches() {
    let x = 10;
    let result: &str = lisp!(cond
        ((> x 5) "big")
        (else "small"));
    assert_eq!(result, "big");
}

#[test]
fn for_pattern_destructuring() {
    let pairs = vec![(1, "one"), (2, "two"), (3, "three")];
    let mut sum = 0;
    lisp!(for (i, _name) in pairs
        (+= sum i));
    assert_eq!(sum, 6);
}

// ── match_expr tests ────────────────────────────────────────

#[test]
fn match_bare_expr() {
    let x = 2;
    let r: &str = lisp!(match x
        (1 => "one")
        (2 => "two")
        (_ => "other"));
    assert_eq!(r, "two");
}

#[test]
fn match_with_lisp_body() {
    let x = 3;
    let r: i32 = lisp!(match x
        (1 => (+ 10 1))
        (2 => (+ 10 2))
        (_ => (+ 10 x)));
    assert_eq!(r, 13);
}

#[test]
fn match_multi_body() {
    let x = 2;
    let result = lisp!(match x
        (1 => (let a 10) (+ a 1))
        (2 => (let b 20) (+ b 2))
        (_ => 0));
    assert_eq!(result, 22);
}

#[test]
fn match_with_guards() {
    let classify = |x: i32| -> &'static str {
        lisp!(match x
            (n if (> n 0) => "positive")
            (n if (< n 0) => "negative")
            (_ => "zero"))
    };
    assert_eq!(classify(5), "positive");
    assert_eq!(classify(-3), "negative");
    assert_eq!(classify(0), "zero");
}

// ── patterns tests ──────────────────────────────────────────

#[test]
fn let_tuple_destructure() {
    lisp!(let (a, b) (tuple 1 2));
    assert_eq!(a, 1);
    assert_eq!(b, 2);
}

#[test]
fn let_nested_tuple_destructure() {
    lisp!(let (a, (b, c)) (tuple 1 (tuple 2 3)));
    assert_eq!(a, 1);
    assert_eq!(b, 2);
    assert_eq!(c, 3);
}

#[test]
fn let_struct_destructure() {
    struct Point {
        x: i32,
        y: i32,
    }
    let p = Point { x: 10, y: 20 };
    lisp!(let Point { x, y } p);
    assert_eq!(x, 10);
    assert_eq!(y, 20);
}

#[test]
fn match_literal_patterns() {
    let val = 2;
    let result = lisp!(match val
        (1 => (10))
        (2 => (20))
        (_ => (0)));
    assert_eq!(result, 20);
}

#[test]
fn match_enum_pattern() {
    let opt: Option<i32> = Some(42);
    let result = lisp!(match opt
        (Some(x) => (x))
        (None => (0)));
    assert_eq!(result, 42);
}

#[test]
fn match_alternative_patterns() {
    let val = 3;
    let result = lisp!(match val
        (1 | 2 => (10))
        (3 | 4 => (20))
        (_ => (0)));
    assert_eq!(result, 20);
}

#[test]
fn match_wildcard() {
    let val = 999;
    let result = lisp!(match val
        (1 => (1))
        (_ => (0)));
    assert_eq!(result, 0);
}

#[test]
fn if_let_some() {
    let opt = Some(7);
    let result = lisp!(if let (Some(x) = opt) (+ x 1) 0);
    assert_eq!(result, 8);
}

#[test]
fn if_let_none() {
    let opt: Option<i32> = None;
    let result = lisp!(if let (Some(x) = opt) (+ x 1) 0);
    assert_eq!(result, 0);
}

#[test]
fn while_let_pattern() {
    lisp!(block
        (let mut stack (vec 1 2 3))
        (let mut sum 0)
        (while let (Some(top) = (stack.pop))
            (+= sum top))
        (assert_eq! 6 sum)
    );
}

#[test]
fn for_in_range_simple() {
    lisp!(let mut sum 0);
    lisp!(for i in (.. 0 5) (+= sum i));
    assert_eq!(sum, 10);
}
