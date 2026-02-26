#![allow(unused_variables)]
#![allow(unused_mut)]
#![allow(unused_assignments)]
#![allow(dead_code)]

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

#[test]
fn let_else_form() {
    let opt: Option<i32> = Some(42);
    lisp!(let else (Some(x) = opt) (panic "expected Some"));
    assert_eq!(x, 42);
}

#[test]
fn let_else_with_return() {
    fn extract(opt: Option<i32>) -> i32 {
        lisp!(let else (Some(x) = opt) (return 0));
        x
    }
    assert_eq!(extract(Some(42)), 42);
    assert_eq!(extract(None), 0);
}

// ── assign tests ────────────────────────────────────────────────────────────

#[test]
fn assign_field() {
    struct P { x: i32 }
    let mut p = P { x: 0 };
    lisp!(= p.x 5);
    assert_eq!(p.x, 5);
}

#[test]
fn assign_index() {
    let mut v = vec![1, 2, 3];
    lisp!(= v[0] 99);
    assert_eq!(v[0], 99);
}

#[test]
fn assign_deref() {
    let mut x = 5;
    let r = &mut x;
    lisp!(= (*r) 10);
    assert_eq!(x, 10);
}

#[test]
fn assign_nested() {
    struct Inner { val: i32 }
    struct Outer { inner: Inner }
    let mut o = Outer { inner: Inner { val: 0 } };
    lisp!(= o.inner.val 42);
    assert_eq!(o.inner.val, 42);
}

#[test]
fn compound_assign_field() {
    struct P { x: i32 }
    let mut p = P { x: 10 };
    lisp!(+= p.x 5);
    assert_eq!(p.x, 15);
}

// ── pattern_let tests ───────────────────────────────────────────────────────

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

#[test]
fn slice_pattern() {
    let arr = [1, 2, 3, 4, 5];
    lisp!(let [first, ..] arr);
    assert_eq!(first, 1);
}

#[test]
fn raw_strings() {
    let s = lisp!(val r#"hello "world""#);
    assert_eq!(s, r#"hello "world""#);
}

// ── complex_let_types tests ─────────────────────────────────────────────────

// ── Primitives & Variables ───────────────────────────────────────────────────

#[test]
fn let_inferred() {
    lisp!(let x 42);
    assert_eq!(x, 42);
}

#[test]
fn let_mut_inferred() {
    lisp!(let mut x 0);
    lisp!(= x 10);
    assert_eq!(x, 10);
}

#[test]
fn let_typed_i32() {
    lisp!(let (x i32) 42);
    assert_eq!(x, 42i32);
}

#[test]
fn let_typed_f64() {
    lisp!(let (x f64) 3.14);
    assert!((x - 3.14f64).abs() < 1e-10);
}

#[test]
fn let_typed_bool() {
    lisp!(let (x bool) true);
    assert_eq!(x, true);
}

#[test]
fn let_typed_str_ref() {
    lisp!(let (s &str) "hello");
    assert_eq!(s, "hello");
}

#[test]
fn let_typed_string() {
    lisp!(let (s String) (String::from "hello"));
    assert_eq!(s, "hello");
}

// ── Arrays & Slices ──────────────────────────────────────────────────────────

#[test]
fn let_typed_array_repeat() {
    // Using raw bracket syntax for array repeat
    let arr: [i32; 5] = [0; 5];
    assert_eq!(arr, [0, 0, 0, 0, 0]);
}

#[test]
fn let_typed_array_literal() {
    let arr: [i32; 3] = lisp!(array 1 2 3);
    assert_eq!(arr, [1, 2, 3]);
}

// ── Tuples ───────────────────────────────────────────────────────────────────

#[test]
fn let_tuple_construction() {
    let t: (i32, f64) = lisp!(tuple 1 2.0);
    assert_eq!(t, (1, 2.0));
}

#[test]
fn let_triple_tuple() {
    let t: (i32, i32, i32) = lisp!(tuple 1 2 3);
    assert_eq!(t, (1, 2, 3));
}

// ── Vectors ──────────────────────────────────────────────────────────────────

#[test]
fn let_typed_vec() {
    lisp!(let (v Vec<i32>) (vec 1 2 3));
    assert_eq!(v, vec![1, 2, 3]);
}

#[test]
fn let_typed_vec_empty() {
    lisp!(let (v Vec<String>) (vec));
    assert!(v.is_empty());
}

// ── References ───────────────────────────────────────────────────────────────

#[test]
fn let_typed_ref() {
    let x = 42;
    lisp!(let (r &i32) (ref x));
    assert_eq!(*r, 42);
}

#[test]
fn let_typed_ref_mut() {
    lisp!(let mut x 0);
    lisp!(let (r &mut i32) (ref mut x));
    *r = 99;
    assert_eq!(x, 99);
}

// ── Option & Result ──────────────────────────────────────────────────────────

#[test]
fn let_typed_option_some() {
    lisp!(let (x Option<i32>) (Some 42));
    assert_eq!(x, Some(42));
}

#[test]
fn let_typed_option_none() {
    let x: Option<i32> = None;
    assert_eq!(x, None);
}

#[test]
fn let_typed_result_ok() {
    let r: Result<i32, String> = Ok(42);
    assert_eq!(r, Ok(42));
}

// ── Arithmetic ───────────────────────────────────────────────────────────────

#[test]
fn let_typed_arithmetic() {
    lisp!(let (x i32) (+ 1 2));
    assert_eq!(x, 3);
}

#[test]
fn let_typed_nested_arithmetic() {
    lisp!(let (x i32) (+ 1 (* 2 3)));
    assert_eq!(x, 7);
}

#[test]
fn let_typed_modulo() {
    let n = 17;
    lisp!(let (x i32) (% n 5));
    assert_eq!(x, 2);
}

// ── Casting ──────────────────────────────────────────────────────────────────

#[test]
fn let_typed_cast_to_f64() {
    let my_int: i32 = 42;
    lisp!(let (x f64) (as my_int f64));
    assert_eq!(x, 42.0f64);
}

#[test]
fn let_typed_cast_to_i32() {
    let my_float: f64 = 3.7;
    lisp!(let (x i32) (as my_float i32));
    assert_eq!(x, 3i32);
}

// ── Chained / Complex ────────────────────────────────────────────────────────

#[test]
fn let_typed_format() {
    let name = "world";
    lisp!(let (s String) (format! "hello {}" name));
    assert_eq!(s, "hello world");
}

#[test]
fn let_typed_index() {
    let my_arr = vec![10, 20, 30];
    lisp!(let (x i32) (index my_arr 1));
    assert_eq!(x, 20);
}

#[test]
fn let_typed_field_access() {
    struct MyStruct { field_name: i32 }
    let my_struct = MyStruct { field_name: 42 };
    lisp!(let (x i32) (. my_struct field_name));
    assert_eq!(x, 42);
}

// ── Destructuring ────────────────────────────────────────────────────────────

#[test]
fn let_tuple_destructure_from_var() {
    let pair = (1, 2);
    lisp!(let (a, b) pair);
    assert_eq!(a, 1);
    assert_eq!(b, 2);
}

// ── Inside functions ─────────────────────────────────────────────────────────

lisp!(
    fn compute ((x i32) (y i32)) i32
    (let (sum i32) (+ x y))
    (let (product i32) (* x y))
    (+ sum product)
);

#[test]
fn let_typed_inside_fn() {
    assert_eq!(compute(3, 4), 19); // 3+4=7, 3*4=12, 7+12=19
}
