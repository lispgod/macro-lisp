#![allow(unused_variables)]
#![allow(unused_mut)]
#![allow(dead_code)]

use macro_lisp::lisp;

// Named function with return type
lisp!(fn add ((x i32) (y i32)) i32
    (+ x y)
);

// Named function with void return
lisp!(fn do_nothing());

// Named function with body
lisp!(fn greet () ()
    (println! "hello")
);

// Public function
lisp!(pub fn pub_multiply ((a i32) (b i32)) i32
    (* a b)
);

// Recursive function
lisp!(fn factorial ((n i32)) i32
    (if (<= n 1)
        1
        (* n (factorial (- n 1)))));

#[test]
fn named_fn_with_return() {
    let result = lisp!(add 3 4);
    assert_eq!(result, 7);
}

#[test]
fn named_fn_void() {
    do_nothing();
    greet();
}

#[test]
fn pub_fn() {
    let result = lisp!(pub_multiply 5 6);
    assert_eq!(result, 30);
}

#[test]
fn recursive_fn() {
    assert_eq!(1, lisp!(factorial 1));
    assert_eq!(120, lisp!(factorial 5));
    assert_eq!(3628800, lisp!(factorial 10));
}

#[test]
fn closure_basic() {
    let f = lisp!(fn ((x i32)) (+ x 1));
    assert_eq!(f(5), 6);
}

#[test]
fn closure_multi_param() {
    let f = lisp!(fn ((a i32) (b i32)) (+ a b));
    assert_eq!(f(3, 4), 7);
}

#[test]
fn closure_no_params() {
    let f = lisp!(fn () (+ 42 0));
    assert_eq!(f(), 42);
}

#[test]
fn closure_move() {
    let val = 42;
    let f = lisp!(fn move () (+ val 0));
    assert_eq!(f(), 42);
}

#[test]
fn closure_as_argument() {
    let f = lisp!(fn ((x i32)) (* x x));
    let results: Vec<i32> = (1..=5).map(f).collect();
    assert_eq!(results, vec![1, 4, 9, 16, 25]);
}

#[test]
fn method_call() {
    let s = String::from("hello");
    let len = lisp!(s.len);
    assert_eq!(len, 5);
}

#[test]
fn path_qualified_call() {
    let s = lisp!(String::from "hello");
    assert_eq!(s, "hello");
}

#[test]
fn function_call_catch_all() {
    fn my_func(a: i32, b: i32) -> i32 {
        a + b
    }
    let result = lisp!(my_func 3 4);
    assert_eq!(result, 7);
}

#[test]
fn closure_with_return_type() {
    let add = lisp!(fn ((a i32) (b i32)) -> i32 (+ a b));
    assert_eq!(add(3, 4), 7);
}

#[test]
fn closure_with_return_type_move() {
    let x = 10;
    let f = lisp!(fn move ((y i32)) -> i32 (+ x y));
    assert_eq!(f(5), 15);
}

#[test]
fn closure_zero_params_return_type() {
    let f = lisp!(fn () -> i32 (+ 1 2));
    assert_eq!(f(), 3);
}
