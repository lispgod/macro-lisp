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

#[test]
fn impl_trait_arg() {
    lisp!(fn print_it ((x &impl core::fmt::Display)) String
        (format! "{}" x));
    assert_eq!(print_it(&42), "42");
}

#[test]
fn dyn_trait_arg() {
    lisp!(fn format_it ((x &dyn core::fmt::Display)) String
        (format! "{}" x));
    assert_eq!(format_it(&42), "42");
}

// --- Merged from generic_fn.rs ---

lisp!(fn max_of<T: PartialOrd> ((a T) (b T)) T
    (if (> a b) a b));

#[test]
fn generic_fn() {
    assert_eq!(max_of(3, 5), 5);
    assert_eq!(max_of(3.14, 2.71), 3.14);
}

lisp!(fn add_em<T> ((a T) (b T)) T
    where (T: core::ops::Add<Output = T>)
    (+ a b));

#[test]
fn generic_fn_where() {
    assert_eq!(add_em(3, 4), 7);
}

// --- Merged from self_params.rs ---
// P10: Self parameter handling in methods

#[test]
fn method_ref_self() {
    struct Counter { val: i32 }
    lisp!(impl Counter
        (fn get ((&self)) i32
            (self.val)));
    let c = Counter { val: 42 };
    assert_eq!(c.get(), 42);
}

#[test]
fn method_mut_self() {
    struct Counter { val: i32 }
    lisp!(impl Counter
        (fn inc ((&mut self))
            (+= self.val 1)));
    let mut c = Counter { val: 0 };
    c.inc();
    c.inc();
    assert_eq!(c.val, 2);
}

#[test]
fn method_owned_self() {
    struct Wrapper { val: i32 }
    lisp!(impl Wrapper
        (fn into_val ((self)) i32
            (self.val)));
    let w = Wrapper { val: 99 };
    assert_eq!(w.into_val(), 99);
}

#[test]
fn method_typed_self() {
    struct Wrapper { val: i32 }
    lisp!(impl Wrapper
        (fn boxed_get ((self Box<Self>)) i32
            (self.val)));
    let w = Box::new(Wrapper { val: 7 });
    assert_eq!(w.boxed_get(), 7);
}

// --- Merged from multi_body.rs ---
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

// --- Merged from untyped_closures.rs ---
// P15: Closures without type annotations

#[test]
fn closure_no_types() {
    lisp!(let add (fn ((x) (y)) (+ x y)));
    assert_eq!(add(3i32, 4i32), 7);
}

// Renamed from closure_no_params to avoid conflict with existing test
#[test]
fn closure_no_params_untyped() {
    lisp!(let five (fn () (+ 5 0)));
    assert_eq!(five(), 5);
}

#[test]
fn closure_zero_params() {
    let f = lisp!(fn () (+ 1 2));
    assert_eq!(f(), 3);
}

#[test]
fn closure_zero_params_move() {
    let x = 42;
    let f = lisp!(fn move () (val x));
    assert_eq!(f(), 42);
}

#[test]
fn closure_zero_params_multi_body() {
    let f = lisp!(fn ()
        (let a 10)
        (let b 20)
        (+ a b));
    assert_eq!(f(), 30);
}

#[test]
fn closure_single_untyped() {
    lisp!(let double (fn ((x)) (* x 2)));
    assert_eq!(double(5i32), 10);
}
