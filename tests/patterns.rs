#![allow(unused_variables)]
#![allow(dead_code)]

use macro_lisp::lisp;

// ── Tuple destructuring in let ──────────────────────────────

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

// ── Struct destructuring in let ─────────────────────────────

#[test]
fn let_struct_destructure() {
    struct Point { x: i32, y: i32 }
    let p = Point { x: 10, y: 20 };
    lisp!(let Point { x, y } p);
    assert_eq!(x, 10);
    assert_eq!(y, 20);
}

// ── Match patterns ──────────────────────────────────────────

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

// ── if let patterns ─────────────────────────────────────────

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

// ── while let patterns ──────────────────────────────────────

#[test]
fn while_let_pattern() {
    lisp!(block
        (let mut stack (vec 1 2 3))
        (let mut sum 0)
        (while let (Some(top) = (stack.pop))
            (+= sum top))
        (macro! assert_eq 6 sum)
    );
}

// ── for with destructuring ──────────────────────────────────

#[test]
fn for_in_range_simple() {
    lisp!(let mut sum 0);
    lisp!(for i in (.. 0 5) (+= sum i));
    assert_eq!(sum, 10);
}
