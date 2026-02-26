#![allow(unused_variables)]
#![allow(unused_mut)]

use macro_lisp::lisp;

#[test]
fn vec_basic() {
    let v = lisp!(vec 1 2 3);
    assert_eq!(v, vec![1, 2, 3]);
}

#[test]
fn vec_empty() {
    let v: Vec<i32> = lisp!(vec);
    assert!(v.is_empty());
}

#[test]
fn vec_with_expressions() {
    let v = lisp!(vec (+ 1 2) (* 3 4) (- 10 5));
    assert_eq!(v, vec![3, 12, 5]);
}

#[test]
fn tuple_basic() {
    let t = lisp!(tuple 1 2 3);
    assert_eq!(t, (1, 2, 3));
}

#[test]
fn tuple_pair() {
    let t = lisp!(tuple 10 20);
    assert_eq!(t, (10, 20));
}

#[test]
fn tuple_single() {
    // (tuple x) produces a single-element tuple (x,)
    let t = lisp!(tuple 42);
    assert_eq!(t, (42,));
}

#[test]
fn new_struct_construction() {
    struct Pos {
        x: i32,
        y: i32,
    }
    let p = lisp!(new Pos (x 10) (y 20));
    assert_eq!(p.x, 10);
    assert_eq!(p.y, 20);
}

#[test]
fn field_access_on_struct() {
    struct Item {
        value: i32,
    }
    let item = Item { value: 99 };
    let v = lisp!(. item value);
    assert_eq!(v, 99);
}

#[test]
fn len_vec() {
    let v = vec![1, 2, 3, 4];
    assert_eq!(lisp!(v.len), 4);
}

#[test]
fn len_string() {
    let s = "hello";
    assert_eq!(lisp!(s.len), 5);
}

#[test]
fn val_bare_value() {
    let x = 42;
    let y = lisp!(val x);
    assert_eq!(y, 42);
}

#[test]
fn val_expression() {
    let y = lisp!(val (+ 1 2));
    assert_eq!(y, 3);
}

#[test]
fn array_repeat() {
    let arr: [i32; 5] = [0; 5];
    assert_eq!(arr, [0, 0, 0, 0, 0]);

    let arr2: [u8; 3] = [42; 3];
    assert_eq!(arr2, [42, 42, 42]);
}

// ── primitives tests ──

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
