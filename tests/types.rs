#![allow(unused_variables)]
#![allow(unused_mut)]
#![allow(dead_code)]

use macro_lisp::lisp;

// struct definitions
lisp!(struct Point
    ((x i32) (y i32))
);

lisp!(struct Color);

lisp!(struct Pair<T>
    ((first T) (second T))
);

lisp!(struct PubFields
    (pub (name String) (age i32))
);

lisp!(struct Mixed
    (pub (name String))
    ((secret i32))
);

#[test]
fn struct_basic() {
    let p = Point { x: 1, y: 2 };
    assert_eq!(p.x, 1);
    assert_eq!(p.y, 2);
}

#[test]
fn struct_unit() {
    let _c = Color;
}

#[test]
fn struct_generic() {
    let p: Pair<i32> = Pair {
        first: 10,
        second: 20,
    };
    assert_eq!(p.first, 10);
    assert_eq!(p.second, 20);
}

#[test]
fn struct_pub_fields() {
    let p = PubFields {
        name: "Alice".to_string(),
        age: 30,
    };
    assert_eq!(p.name, "Alice");
    assert_eq!(p.age, 30);
}

#[test]
fn struct_mixed_fields() {
    let m = Mixed {
        name: "Bob".to_string(),
        secret: 42,
    };
    assert_eq!(m.name, "Bob");
}

// enum definitions
lisp!(
    enum Direction {
        North,
        South,
        East,
        West,
    }
);

#[test]
fn enum_basic() {
    let d = Direction::North;
    let result = match d {
        Direction::North => 1,
        Direction::South => 2,
        Direction::East => 3,
        Direction::West => 4,
    };
    assert_eq!(result, 1);
}

// impl blocks
lisp!(impl Point
    (fn origin () Point
        (new Point (x 0) (y 0)))
);

#[test]
fn impl_method() {
    let p = Point::origin();
    assert_eq!(p.x, 0);
    assert_eq!(p.y, 0);
}

// type alias
lisp!(type Integer = i32);

#[test]
fn type_alias() {
    let x: Integer = 42;
    assert_eq!(x, 42i32);
}

// new (struct construction)
#[test]
fn new_struct() {
    let p = lisp!(new Point (x 10) (y 20));
    assert_eq!(p.x, 10);
    assert_eq!(p.y, 20);
}

#[test]
fn new_struct_with_expr() {
    let p = lisp!(new Point (x (+ 1 2)) (y (* 3 4)));
    assert_eq!(p.x, 3);
    assert_eq!(p.y, 12);
}

// field access
#[test]
fn field_access() {
    let p = Point { x: 42, y: 99 };
    let x = lisp!(. p x);
    assert_eq!(x, 42);
}

// const
lisp!(const MAX_SIZE usize = 100);

#[test]
fn const_value() {
    assert_eq!(MAX_SIZE, 100);
}

// ── structs tests ──

lisp!(#[derive(Debug)]
 struct Bounded<T: core::fmt::Debug + Clone> ((val T)));

#[test]
fn struct_with_bounds() {
    let b = Bounded { val: 42 };
    assert_eq!(format!("{:?}", b), "Bounded { val: 42 }");
}

lisp!(struct StrRef<'a> ((s &'a str)));

#[test]
fn struct_with_lifetime() {
    let s = String::from("hello");
    let r = StrRef { s: &s };
    assert_eq!(r.s, "hello");
}

// ── enums tests ──

lisp!(#[derive(Debug, PartialEq)]
 enum Shape
    (Circle f64)
    (Rect ((w f64) (h f64)))
    (Unit));

#[test]
fn enum_all_variant_kinds() {
    let c = Shape::Circle(3.14);
    let r = Shape::Rect { w: 10.0, h: 20.0 };
    let u = Shape::Unit;
    assert_eq!(c, Shape::Circle(3.14));
    assert_eq!(r, Shape::Rect { w: 10.0, h: 20.0 });
    assert_eq!(u, Shape::Unit);
}

lisp!(#[derive(Debug)]
 enum Maybe<T>
    (Just T)
    (Nothing));

#[test]
fn enum_generic() {
    let x: Maybe<i32> = Maybe::Just(42);
    match x {
        Maybe::Just(v) => assert_eq!(v, 42),
        Maybe::Nothing => panic!("expected Just"),
    }
}

// ── traits tests ──

lisp!(trait Describable
    (fn name ((self &Self)) &str)
    (fn describe ((self &Self)) String
        (format! "I am {}" (Describable::name self))));

lisp!(struct Cat);
lisp!(impl Describable for Cat
    (fn name ((self &Self)) &str "Cat"));

#[test]
fn trait_with_default_method() {
    let c = Cat;
    assert_eq!(c.describe(), "I am Cat");
}

lisp!(trait Named : core::fmt::Debug
    (fn name ((self &Self)) &str));

#[test]
fn trait_with_supertrait() {
    // Verify that Named requires Debug bound
}

lisp!(trait Container
    (type Item)
    (fn first ((self &Self)) Option<&Self::Item>));

#[test]
fn trait_with_associated_type() {
    // Verify trait compiles
}

// ── struct_lit tests ──

#[test]
fn struct_lit_spread() {
    #[derive(Debug, PartialEq)]
    struct Point {
        x: i32,
        y: i32,
    }
    let default = Point { x: 0, y: 0 };
    lisp!(let p (new Point (x 5) (.. default)));
    assert_eq!(p, Point { x: 5, y: 0 });
}

#[test]
fn struct_lit_shorthand() {
    #[derive(Debug, PartialEq)]
    struct Point {
        x: i32,
        y: i32,
    }
    let x = 10;
    let y = 20;
    lisp!(let p (new Point x y));
    assert_eq!(p, Point { x: 10, y: 20 });
}

#[test]
fn new_spread_in_proc_macro() {
    #[derive(Debug, PartialEq, Clone)]
    struct Config {
        width: i32,
        height: i32,
        depth: i32,
    }

    let base = Config {
        width: 10,
        height: 20,
        depth: 30,
    };
    let base2 = base.clone();
    lisp!(let updated (new Config (width 100) (.. base2)));
    assert_eq!(updated.width, 100);
    assert_eq!(updated.height, 20);
    assert_eq!(updated.depth, 30);
}

#[test]
fn new_tuple_struct() {
    struct Pair(i32, i32);
    lisp!(let p (new Pair 10 20));
    assert_eq!(p.0, 10);
    assert_eq!(p.1, 20);
}

#[test]
fn new_enum_variant() {
    #[derive(Debug, PartialEq)]
    enum Shape {
        Circle { radius: f64 },
        Rect { w: f64, h: f64 },
    }

    lisp!(let c (new Shape::Circle (radius 5.0)));
    assert_eq!(c, Shape::Circle { radius: 5.0 });

    lisp!(let r (new Shape::Rect (w 3.0) (h 4.0)));
    assert_eq!(r, Shape::Rect { w: 3.0, h: 4.0 });
}

// ── where_clauses tests ──

#[test]
fn struct_where() {
    lisp!(#[derive(Debug)]
     struct Holder<T> where (T: core::fmt::Debug)
        ((val T)));
    let h = Holder { val: 42 };
    assert_eq!(format!("{:?}", h), "Holder { val: 42 }");
}

#[test]
fn impl_where() {
    struct Pair<T> {
        a: T,
        b: T,
    }
    lisp!(impl<T> Pair<T> where (T: core::ops::Add<Output = T> + Copy)
        (fn sum ((&self)) T
            (+ (self.a) (self.b))));
    let p = Pair { a: 3, b: 4 };
    assert_eq!(p.sum(), 7);
}

// ── impl_blocks tests ──

#[derive(Debug, PartialEq)]
struct Wrapper<T> {
    value: T,
}

lisp!(impl<T: Copy> Wrapper<T>
    (fn get ((self &Self)) T
        (self.value)));

#[test]
fn impl_with_generics() {
    let w = Wrapper { value: 42 };
    assert_eq!(w.get(), 42);
}

lisp!(struct MyNum ((val i32)));

lisp!(impl core::fmt::Display for MyNum
    (fn fmt ((self &Self) (f &mut core::fmt::Formatter)) core::fmt::Result
        (write! f "{}" (self.val))));

#[test]
fn impl_trait_with_path() {
    let n = MyNum { val: 42 };
    assert_eq!(format!("{}", n), "42");
}

#[derive(Debug, Clone, Copy, PartialEq)]
struct V2 {
    x: i32,
    y: i32,
}

lisp!(impl core::ops::Add for V2
    (type Output = V2)
    (fn add ((self Self) (rhs Self)) V2
        (new V2 (x (+ (self.x) (. rhs x))) (y (+ (self.y) (. rhs y))))));

#[test]
fn impl_add_with_associated_type() {
    let a = V2 { x: 1, y: 2 };
    let b = V2 { x: 3, y: 4 };
    assert_eq!(a + b, V2 { x: 4, y: 6 });
}

#[test]
fn impl_with_lifetime() {
    struct StrRef<'a> {
        s: &'a str,
    }

    lisp!(impl<'a> StrRef<'a>
        (fn value ((&self)) &'a str
            (self.s)));

    let sr = StrRef { s: "hello" };
    assert_eq!(sr.value(), "hello");
}

#[test]
fn method_call_chaining_no_args() {
    // Each non-self dot segment should be a zero-arg method call
    // result.unwrap.to_string → result.unwrap().to_string()
    let result: Result<i32, &str> = Ok(42);
    let s: String = lisp!(result.unwrap.to_string);
    assert_eq!(s, "42");
}

#[test]
fn method_chain_in_proc_macro_fn() {
    // Test the proc macro path (inside lisp-defined fn)
    lisp!(fn get_len ((s &str)) usize
        (s.len));

    assert_eq!(get_len("hello"), 5);
}

// ── core_traits tests ──

#[test]
fn core_ops_add() {
    #[derive(Debug, Clone, Copy, PartialEq)]
    struct Vec2 {
        x: i32,
        y: i32,
    }
    lisp!(impl core::ops::Add for Vec2
        (type Output = Vec2)
        (fn add ((self Self) (rhs Self)) Vec2
            (new Vec2 (x (+ (self.x) (. rhs x))) (y (+ (self.y) (. rhs y))))));
    let a = Vec2 { x: 1, y: 2 };
    let b = Vec2 { x: 3, y: 4 };
    assert_eq!(a + b, Vec2 { x: 4, y: 6 });
}

#[test]
fn core_fmt_display() {
    struct Pair {
        a: i32,
        b: i32,
    }
    lisp!(impl core::fmt::Display for Pair
        (fn fmt ((self &Self) (f &mut core::fmt::Formatter)) core::fmt::Result
            (write! f "({}, {})" (self.a) (self.b))));
    let p = Pair { a: 3, b: 7 };
    assert_eq!(format!("{}", p), "(3, 7)");
}

#[test]
fn core_default() {
    #[derive(Debug, PartialEq)]
    struct Point {
        x: i32,
        y: i32,
    }
    lisp!(impl core::default::Default for Point
        (fn default () Point
            (new Point (x 0) (y 0))));
    let p: Point = Default::default();
    assert_eq!(p, Point { x: 0, y: 0 });
}

#[test]
fn core_convert_from() {
    struct Celsius {
        val: f64,
    }
    struct Fahrenheit {
        val: f64,
    }
    lisp!(impl core::convert::From<Celsius> for Fahrenheit
        (fn from ((c Celsius)) Fahrenheit
            (new Fahrenheit (val (+ (* (. c val) 1.8) 32.0)))));
    let f: Fahrenheit = Celsius { val: 100.0 }.into();
    assert!((f.val - 212.0).abs() < f64::EPSILON);
}

#[test]
fn core_clone() {
    struct MyVal {
        n: i32,
    }
    lisp!(impl core::clone::Clone for MyVal
        (fn clone ((self &Self)) MyVal
            (new MyVal (n (self.n)))));
    let a = MyVal { n: 42 };
    let b = a.clone();
    assert_eq!(b.n, 42);
}

#[test]
fn core_ops_neg() {
    #[derive(Debug, PartialEq)]
    struct Num {
        val: i32,
    }
    lisp!(impl core::ops::Neg for Num
        (type Output = Num)
        (fn neg ((self Self)) Num
            (new Num (val (neg (self.val))))));
    let n = Num { val: 5 };
    assert_eq!(-n, Num { val: -5 });
}

#[test]
fn core_ops_drop() {
    use std::cell::Cell;
    struct Guard<'a> {
        dropped: &'a Cell<bool>,
    }
    lisp!(impl<'a> core::ops::Drop for Guard<'a>
        (fn drop ((&mut self))
            (rust self.dropped.set(true))));
    let flag = Cell::new(false);
    {
        let _g = Guard { dropped: &flag };
    }
    assert!(flag.get());
}

#[test]
fn core_cmp_partial_eq() {
    struct Approx {
        val: f64,
    }
    lisp!(impl core::cmp::PartialEq for Approx
        (fn eq ((self &Self) (other &Self)) bool
            (rust { (self.val - other.val).abs() < 0.01 })));
    let a = Approx { val: 1.0 };
    let b = Approx { val: 1.005 };
    let c = Approx { val: 2.0 };
    assert!(a == b);
    assert!(a != c);
}

#[test]
fn core_iter_iterator() {
    struct Counter {
        current: i32,
        max: i32,
    }
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
