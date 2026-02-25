#![allow(unused_variables)]
#![allow(dead_code)]

use macro_lisp::lisp;

// 1. core::ops::Add
#[test]
fn core_ops_add() {
    #[derive(Debug, Clone, Copy, PartialEq)]
    struct Vec2 { x: i32, y: i32 }
    lisp!(impl core::ops::Add for Vec2
        (type Output = Vec2)
        (fn add ((self Self) (rhs Self)) Vec2
            (new Vec2 (x (+ (self.x) (. rhs x))) (y (+ (self.y) (. rhs y))))));
    let a = Vec2 { x: 1, y: 2 };
    let b = Vec2 { x: 3, y: 4 };
    assert_eq!(a + b, Vec2 { x: 4, y: 6 });
}

// 2. core::fmt::Display
#[test]
fn core_fmt_display() {
    struct Pair { a: i32, b: i32 }
    lisp!(impl core::fmt::Display for Pair
        (fn fmt ((self &Self) (f &mut core::fmt::Formatter)) core::fmt::Result
            (write! f "({}, {})" (self.a) (self.b))));
    let p = Pair { a: 3, b: 7 };
    assert_eq!(format!("{}", p), "(3, 7)");
}

// 3. core::default::Default
#[test]
fn core_default() {
    #[derive(Debug, PartialEq)]
    struct Point { x: i32, y: i32 }
    lisp!(impl core::default::Default for Point
        (fn default () Point
            (new Point (x 0) (y 0))));
    let p: Point = Default::default();
    assert_eq!(p, Point { x: 0, y: 0 });
}

// 4. core::convert::From
#[test]
fn core_convert_from() {
    struct Celsius { val: f64 }
    struct Fahrenheit { val: f64 }
    lisp!(impl core::convert::From<Celsius> for Fahrenheit
        (fn from ((c Celsius)) Fahrenheit
            (new Fahrenheit (val (+ (* (. c val) 1.8) 32.0)))));
    let f: Fahrenheit = Celsius { val: 100.0 }.into();
    assert!((f.val - 212.0).abs() < f64::EPSILON);
}

// 5. core::clone::Clone (manual impl)
#[test]
fn core_clone() {
    struct MyVal { n: i32 }
    lisp!(impl core::clone::Clone for MyVal
        (fn clone ((self &Self)) MyVal
            (new MyVal (n (self.n)))));
    let a = MyVal { n: 42 };
    let b = a.clone();
    assert_eq!(b.n, 42);
}

// 6. core::ops::Neg
#[test]
fn core_ops_neg() {
    #[derive(Debug, PartialEq)]
    struct Num { val: i32 }
    lisp!(impl core::ops::Neg for Num
        (type Output = Num)
        (fn neg ((self Self)) Num
            (new Num (val (neg (self.val))))));
    let n = Num { val: 5 };
    assert_eq!(-n, Num { val: -5 });
}

// 7. core::ops::Drop
#[test]
fn core_ops_drop() {
    use std::cell::Cell;
    struct Guard<'a> { dropped: &'a Cell<bool> }
    lisp!(impl<'a> core::ops::Drop for Guard<'a>
        (fn drop ((&mut self))
            (rust self.dropped.set(true))));
    let flag = Cell::new(false);
    { let _g = Guard { dropped: &flag }; }
    assert!(flag.get());
}

// 8. core::cmp::PartialEq (manual impl)
#[test]
fn core_cmp_partial_eq() {
    struct Approx { val: f64 }
    lisp!(impl core::cmp::PartialEq for Approx
        (fn eq ((self &Self) (other &Self)) bool
            (rust { (self.val - other.val).abs() < 0.01 })));
    let a = Approx { val: 1.0 };
    let b = Approx { val: 1.005 };
    let c = Approx { val: 2.0 };
    assert!(a == b);
    assert!(a != c);
}

// 9. core::iter::Iterator
#[test]
fn core_iter_iterator() {
    struct Counter { current: i32, max: i32 }
    lisp!(impl core::iter::Iterator for Counter
        (type Item = i32)
        (fn next ((&mut self)) Option<i32>
            (if (< (self.current) (self.max))
                (block
                    (let val (self.current))
                    (+= self.current 1)
                    (Some val))
                None)));
    let sum: i32 = Counter { current: 0, max: 5 }.sum();
    assert_eq!(sum, 10);
    let collected: Vec<i32> = Counter { current: 0, max: 4 }.collect();
    assert_eq!(collected, vec![0, 1, 2, 3]);
}
