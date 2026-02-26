// `tests/showcase.rs` — Comprehensive feature showcase for macro-lisp.
//
// A single connected program exercising **every implemented feature**.
// Every `assert!` / `assert_eq!` verifies a computed value.
// Features that don't work yet are commented with `// TODO: not yet implemented`.

#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_mut)]
#![allow(unused_assignments)]
#![allow(unused_imports)]
#![allow(unused_labels)]

use macro_lisp::lisp;

// =============================================================================
// 1. Struct Definitions
// =============================================================================

// Named-field struct
lisp!(struct Point ((x i32) (y i32)));

// Tuple struct
lisp!(struct Meters (f64));

// Unit struct
lisp!(struct Marker);

// Generic struct with trait bounds
lisp!(#[derive(Debug)]
 struct Container<T: core::fmt::Debug + Clone> ((val T)));

// Struct with lifetime
lisp!(struct StrRef<'a> ((s &'a str)));

// Public struct with pub fields
lisp!(pub struct Color (pub (r u8) (g u8) (b u8)));

// Struct with where clause
lisp!(struct Holder<T> where (T: Clone + Default) ((inner T)));

// =============================================================================
// 2. Enum Definitions
// =============================================================================

// Unit variants
lisp!(#[derive(Debug, PartialEq, Clone)]
 enum Direction
    (North)
    (South)
    (East)
    (West));

// Tuple + struct + unit variants
lisp!(#[derive(Debug, PartialEq)]
 enum Shape
    (Circle f64)
    (Rect ((w f64) (h f64)))
    (Unit));

// Generic enum
lisp!(#[derive(Debug, PartialEq)]
 enum Maybe<T>
    (Just T)
    (Nothing));

// =============================================================================
// 3. Trait Definitions
// =============================================================================

// Simple trait with a method declaration (no body)
lisp!(trait Describable
    (fn describe ((&self)) String));

// Trait with default method implementation
lisp!(trait HasArea
    (fn area ((&self)) f64));

// Trait with supertraits
lisp!(trait Printable : core::fmt::Display
    (fn print ((&self))
        (println! "{}" self)));

// Trait with associated type
// TODO: associated type defaults (type Output = i32) require nightly;
// use associated type without default instead.
lisp!(trait Producer
    (type Output)
    (fn produce ((&self)) i32));

// =============================================================================
// 4. Impl Blocks & Trait Implementations
// =============================================================================

// Inherent impl with methods
lisp!(impl Point
    (fn new ((x i32) (y i32)) Point
        (new Point (x x) (y y)))
    (fn origin () Point
        (new Point (x 0) (y 0)))
    (fn translate ((&self) (dx i32) (dy i32)) Point
        (new Point (x (+ (self.x) dx)) (y (+ (self.y) dy)))));

// Trait impl: core::fmt::Display for Point
lisp!(impl core::fmt::Display for Point
    (fn fmt ((self &Self) (f &mut core::fmt::Formatter)) core::fmt::Result
        (write! f "({}, {})" (self.x) (self.y))));

// Trait impl: Describable for Point
lisp!(impl Describable for Point
    (fn describe ((&self)) String
        (format! "Point({}, {})" (self.x) (self.y))));

// Trait impl: HasArea for Shape
lisp!(impl HasArea for Shape
    (fn area ((&self)) f64
        (rust {
            match self {
                Shape::Circle(r) => std::f64::consts::PI * r * r,
                Shape::Rect { w, h } => w * h,
                Shape::Unit => 0.0,
            }
        })));

// Trait impl: core::ops::Add for Point
lisp!(impl core::ops::Add for Point
    (type Output = Point)
    (fn add ((self Self) (rhs Self)) Point
        (new Point (x (+ (self.x) (. rhs x))) (y (+ (self.y) (. rhs y))))));

// Trait impl: core::default::Default for Point
lisp!(impl core::default::Default for Point
    (fn default () Point
        (new Point (x 0) (y 0))));

// Trait impl: core::clone::Clone for Point
lisp!(impl core::clone::Clone for Point
    (fn clone ((&self)) Point
        (new Point (x (self.x)) (y (self.y)))));

// Trait impl: core::cmp::PartialEq for Point
lisp!(impl core::cmp::PartialEq for Point
    (fn eq ((self &Self) (other &Self)) bool
        (rust { self.x == other.x && self.y == other.y })));

// Trait impl: core::ops::Neg for Point
lisp!(impl core::ops::Neg for Point
    (type Output = Point)
    (fn neg ((self Self)) Point
        (new Point (x (neg (self.x))) (y (neg (self.y))))));

// Trait impl: core::ops::Drop — verify drop is called
lisp!(struct DropCounter ((count &'static std::cell::Cell<u32>)));
lisp!(impl core::ops::Drop for DropCounter
    (fn drop ((&mut self))
        (rust {
            self.count.set(self.count.get() + 1);
        })));

// Trait impl: core::convert::From
lisp!(impl core::convert::From<(i32, i32)> for Point
    (fn from ((t (i32, i32))) Point
        (rust { Point { x: t.0, y: t.1 } })));

// Iterator impl
lisp!(struct Counter ((current i32) (max i32)));
lisp!(impl core::iter::Iterator for Counter
    (type Item = i32)
    (fn next ((&mut self)) Option<i32>
        (if (< (self.current) (self.max))
            (block
                (let val (self.current))
                (+= self.current 1)
                (Some val))
            None)));

// Generic impl
lisp!(impl<T: core::fmt::Debug + Clone> Container<T>
    (fn unwrap ((&self)) T
        (rust { self.val.clone() })));

// =============================================================================
// 5. Named Functions
// =============================================================================

// Basic named function
lisp!(fn add ((a i32) (b i32)) i32
    (+ a b));

// Recursive function
lisp!(fn factorial ((n i32)) i32
    (if (<= n 1)
        1
        (* n (factorial (- n 1)))));

// Function with multiple body statements
lisp!(fn sum_range ((start i32) (end i32)) i32
    (let mut total 0)
    (for i in (.. start end)
        (+= total i))
    (+ total 0));

// Const function
lisp!(const fn const_add ((a i32) (b i32)) i32
    (+ a b));

// Unsafe function
lisp!(unsafe fn unsafe_double ((x i32)) i32
    (* x 2));

// Extern function
lisp!(extern "C" fn extern_add ((a i32) (b i32)) i32
    (+ a b));

// Public function
lisp!(pub fn pub_square ((x i32)) i32
    (* x x));

// Generic function with trait bounds
lisp!(fn generic_add <T: core::ops::Add<Output = T>> ((a T) (b T)) T
    (+ a b));

// Generic function with where clause
lisp!(fn debug_value <T> ((val &T)) String where (T: core::fmt::Debug)
    (format! "{:?}" val));

// Void function (no return type)
lisp!(fn void_fn ()
    (let _x 42));

// =============================================================================
// 6. Closures
// =============================================================================

#[test]
fn closures_typed() {
    let add_closure = lisp!(fn ((a i32) (b i32)) (+ a b));
    assert_eq!(add_closure(3, 4), 7);
}

#[test]
fn closures_untyped() {
    let double = lisp!(fn ((x)) (* x 2));
    assert_eq!(double(5), 10);
}

#[test]
fn closures_move() {
    let captured = 42;
    let get_val = lisp!(fn move ((x i32)) (+ x captured));
    assert_eq!(get_val(8), 50);
}

#[test]
fn closures_no_params() {
    // TODO: (fn () expr) doesn't work as zero-param closure — matches named fn pattern
    // Workaround: use Rust closure directly
    let make_42 = || 42;
    assert_eq!(make_42(), 42);
}

// =============================================================================
// 7. Operators
// =============================================================================

#[test]
fn arithmetic_binary() {
    assert_eq!(lisp!(+ 10 20), 30);
    assert_eq!(lisp!(- 50 15), 35);
    assert_eq!(lisp!(* 6 7), 42);
    assert_eq!(lisp!(/ 100 4), 25);
    assert_eq!(lisp!(% 17 5), 2);
}

#[test]
fn arithmetic_variadic() {
    assert_eq!(lisp!(+ 1 2 3 4 5), 15);
    assert_eq!(lisp!(- 100 10 20 30), 40);
    assert_eq!(lisp!(* 2 3 4), 24);
}

#[test]
fn arithmetic_nested() {
    assert_eq!(lisp!(+ (* 3 4) (- 10 5)), 17);
}

#[test]
fn comparison_operators() {
    assert!(lisp!(== 5 5));
    assert!(lisp!(!= 5 6));
    assert!(lisp!(< 1 2));
    assert!(lisp!(> 2 1));
    assert!(lisp!(<= 3 3));
    assert!(lisp!(>= 4 3));
}

#[test]
fn logical_operators() {
    assert!(lisp!(&& true true));
    assert!(!lisp!(&& true false));
    assert!(lisp!(|| false true));
    assert!(!lisp!(|| false false));
    assert!(lisp!(! false));
    assert!(!lisp!(! true));
}

#[test]
fn bitwise_operators() {
    assert_eq!(lisp!(& 0b1100u8 0b1010u8), 0b1000u8);
    assert_eq!(lisp!(| 0b1100u8 0b1010u8), 0b1110u8);
    assert_eq!(lisp!(^ 0b1100u8 0b1010u8), 0b0110u8);
    assert_eq!(lisp!(<< 1u8 4), 16u8);
    assert_eq!(lisp!(>> 16u8 2), 4u8);
}

#[test]
fn unary_negation_and_not() {
    let x = 5;
    assert_eq!(lisp!(neg x), -5);
    assert_eq!(lisp!(! true), false);
}

// =============================================================================
// 8. Control Flow
// =============================================================================

#[test]
fn if_else() {
    let result = lisp!(if (> 5 3) "yes" "no");
    assert_eq!(result, "yes");
}

#[test]
fn if_only() {
    lisp!(let mut x 0);
    lisp!(if (> 5 3) (= x 1));
    assert_eq!(x, 1);
}

#[test]
fn if_let_form() {
    let opt: Option<i32> = Some(42);
    let result = lisp!(if let (Some(val) = opt) val 0);
    assert_eq!(result, 42);
}

#[test]
fn match_expression() {
    let label: &str = lisp!(match 42
        (0 => "zero")
        (1 => "one")
        (_ => "other"));
    assert_eq!(label, "other");
}

#[test]
fn match_with_alternatives() {
    let x = 2;
    let label: &str = lisp!(match x
        (1 | 2 => "small")
        (_ => "big"));
    assert_eq!(label, "small");
}

#[test]
fn while_loop() {
    lisp!(let mut n 0);
    lisp!(while (< n 5) (+= n 1));
    assert_eq!(n, 5);
}

#[test]
fn while_let_form() {
    let mut stack = vec![1, 2, 3];
    lisp!(let mut sum 0);
    lisp!(while let (Some(val) = (stack.pop))
        (+= sum val));
    assert_eq!(sum, 6);
}

#[test]
fn for_in_loop() {
    lisp!(let mut sum 0);
    lisp!(for i in (.. 0 5) (+= sum i));
    assert_eq!(sum, 10);
}

#[test]
fn loop_with_break_value() {
    let result = lisp!(loop (break 42));
    assert_eq!(result, 42);
}

#[test]
fn loop_with_break_and_continue() {
    lisp!(let mut n 0);
    lisp!(let mut sum 0);
    lisp!(loop
        (+= n 1)
        (if (>= n 10) (break))
        (if (== (% n 2) 0) (continue))
        (+= sum n));
    // sum of odd numbers 1..9: 1+3+5+7+9 = 25
    assert_eq!(sum, 25);
}

#[test]
fn labeled_loops() {
    lisp!(let mut found false);
    lisp!('outer for i in (.. 0 5)
        ('inner for j in (.. 0 5)
            (if (&& (== i 2) (== j 3))
                (block
                    (= found true)
                    (break 'outer)))));
    assert!(found);
}

#[test]
fn labeled_while() {
    lisp!(let mut sum 0);
    lisp!(let mut i 0);
    lisp!('w while (< i 5)
        (+= i 1)
        (+= sum i));
    assert_eq!(sum, 15);
}

#[test]
fn labeled_for() {
    lisp!(let mut total 0);
    lisp!('f for i in (.. 0 3)
        (+= total i));
    assert_eq!(total, 3);
}

#[test]
fn labeled_block() {
    let result: i32 = lisp!(block 'blk
        (break 'blk 42));
    assert_eq!(result, 42);
}

#[test]
fn block_expression() {
    let x = lisp!(block
        (let a 10)
        (let b 20)
        (+ a b));
    assert_eq!(x, 30);
}

#[test]
fn return_from_function() {
    lisp!(fn early_return ((x i32)) i32
        (if (< x 0) (return 0))
        (+ x 1));
    assert_eq!(early_return(5), 6);
    assert_eq!(early_return(-1), 0);
}

// =============================================================================
// 9. Bindings & Assignment
// =============================================================================

#[test]
fn let_immutable() {
    lisp!(let x 42);
    assert_eq!(x, 42);
}

#[test]
fn let_mutable() {
    lisp!(let mut y 0);
    lisp!(= y 10);
    assert_eq!(y, 10);
}

#[test]
fn let_typed() {
    lisp!(let (z i32) (+ 1 2));
    assert_eq!(z, 3);
}

#[test]
fn let_mut_typed() {
    lisp!(let mut (w i32) 0);
    lisp!(= w 99);
    assert_eq!(w, 99);
}

#[test]
fn let_expression_value() {
    lisp!(let r (+ (* 3 4) 2));
    assert_eq!(r, 14);
}

#[test]
fn scoped_bindings() {
    let result = lisp!(let ((a 10) (b 20) (c 30))
        (= a (+ a b))
        (+ a c));
    // a = 10+20=30, result = 30+30=60
    assert_eq!(result, 60);
}

#[test]
fn compound_assignments() {
    lisp!(let mut x 100);
    lisp!(+= x 10);
    assert_eq!(x, 110);
    lisp!(-= x 20);
    assert_eq!(x, 90);
    lisp!(*= x 2);
    assert_eq!(x, 180);
    lisp!(/= x 3);
    assert_eq!(x, 60);
    lisp!(%= x 7);
    assert_eq!(x, 4);
}

#[test]
fn bitwise_compound_assignments() {
    lisp!(let mut x 0b1111u8);
    lisp!(&= x 0b1010);
    assert_eq!(x, 0b1010u8);
    lisp!(|= x 0b0101);
    assert_eq!(x, 0b1111u8);
    lisp!(^= x 0b1010);
    assert_eq!(x, 0b0101u8);
    lisp!(let mut y 1u8);
    lisp!(<<= y 3);
    assert_eq!(y, 8u8);
    lisp!(>>= y 2);
    assert_eq!(y, 2u8);
}

#[test]
fn destructuring_let() {
    let pair = (10, 20);
    lisp!(let (a, b) pair);
    assert_eq!(a, 10);
    assert_eq!(b, 20);
}

#[test]
fn struct_destructuring_let() {
    let p = Point::new(5, 10);
    lisp!(let Point { x, y } p);
    assert_eq!(x, 5);
    assert_eq!(y, 10);
}

// =============================================================================
// 10. Const and Static
// =============================================================================

lisp!(const MY_CONST i32 = 42);
lisp!(const MY_CONST2 i32 100);
lisp!(static MY_STATIC i32 = 200);
lisp!(static MY_STATIC2 i32 300);
lisp!(static mut MY_MUT_STATIC i32 = 0);

#[test]
fn const_values() {
    assert_eq!(MY_CONST, 42);
    assert_eq!(MY_CONST2, 100);
}

#[test]
fn static_values() {
    assert_eq!(MY_STATIC, 200);
    assert_eq!(MY_STATIC2, 300);
}

#[test]
fn static_mut_values() {
    unsafe {
        MY_MUT_STATIC = 42;
        assert_eq!(MY_MUT_STATIC, 42);
    }
}

#[test]
fn const_fn_usage() {
    const RESULT: i32 = const_add(2, 3);
    assert_eq!(RESULT, 5);
}

// =============================================================================
// 11. Construction (new, tuple, array, vec)
// =============================================================================

#[test]
fn new_struct_construction() {
    let p = lisp!(new Point (x 10) (y 20));
    assert_eq!(p.x, 10);
    assert_eq!(p.y, 20);
}

#[test]
fn new_struct_shorthand() {
    let x = 5;
    let y = 10;
    let p = lisp!(new Point x y);
    assert_eq!(p.x, 5);
    assert_eq!(p.y, 10);
}

#[test]
fn new_struct_spread() {
    let base = Point { x: 1, y: 2 };
    let p = lisp!(new Point (x 99) (.. base));
    assert_eq!(p.x, 99);
    assert_eq!(p.y, 2);
}

#[test]
fn tuple_construction() {
    let t0: () = lisp!(tuple);
    let t1: (i32,) = lisp!(tuple 42);
    let t3: (i32, i32, i32) = lisp!(tuple 1 2 3);
    assert_eq!(t1, (42,));
    assert_eq!(t3, (1, 2, 3));
}

#[test]
fn array_construction() {
    let arr: [i32; 3] = lisp!(array 10 20 30);
    assert_eq!(arr, [10, 20, 30]);
    let empty: [i32; 0] = lisp!(array);
    assert_eq!(empty, []);
}

#[test]
fn vec_construction() {
    let v = lisp!(vec 1 2 3);
    assert_eq!(v, vec![1, 2, 3]);
    let empty: Vec<i32> = lisp!(vec);
    assert_eq!(empty, Vec::<i32>::new());
}

// =============================================================================
// 12. References, Casting, Access
// =============================================================================

#[test]
fn ref_and_deref() {
    let x = 42;
    let r: &i32 = lisp!(ref x);
    assert_eq!(lisp!(deref r), 42);
}

#[test]
fn ref_mut() {
    lisp!(let mut x 0);
    let r: &mut i32 = lisp!(ref mut x);
    *r = 99;
    assert_eq!(x, 99);
}

#[test]
fn as_casting() {
    let x: i32 = 65;
    let c: u8 = lisp!(as x u8);
    assert_eq!(c, 65u8);
}

#[test]
fn try_operator() {
    fn parse_num(s: &str) -> Result<i32, std::num::ParseIntError> {
        let val: Result<i32, _> = s.parse();
        let n = lisp!(? val);
        Ok(lisp!(+ n 1))
    }
    assert_eq!(parse_num("5").unwrap(), 6);
    assert!(parse_num("abc").is_err());
}

#[test]
fn index_access() {
    let v = vec![10, 20, 30];
    assert_eq!(lisp!(index v 1), 20);
}

#[test]
fn dot_field_access() {
    let p = Point::new(3, 7);
    assert_eq!(lisp!(. p x), 3);
    assert_eq!(lisp!(. p y), 7);
}

#[test]
fn dot_chained_field_access() {
    struct Inner { val: i32 }
    struct Outer { inner: Inner }
    let o = Outer { inner: Inner { val: 42 } };
    assert_eq!(lisp!(. o inner val), 42);
}

#[test]
fn ranges() {
    // Exclusive range
    lisp!(let mut sum 0);
    for i in lisp!(.. 0 5) { sum += i; }
    assert_eq!(sum, 10);

    // Inclusive range
    lisp!(let mut sum2 0);
    for i in lisp!(..= 0 5) { sum2 += i; }
    assert_eq!(sum2, 15);

    // Half-open range for slicing
    let v = vec![10, 20, 30, 40, 50];
    let slice = &v[lisp!(.. 2)];
    assert_eq!(slice, &[30, 40, 50]);

    // Full range
    let all = &v[lisp!(..)];
    assert_eq!(all, &[10, 20, 30, 40, 50]);
}

// =============================================================================
// 13. Macro Invocations
// =============================================================================

#[test]
fn macro_println() {
    // Smoke test — just confirm it compiles and runs
    lisp!(println! "showcase println: {}" 42);
}

#[test]
fn macro_format() {
    let s = lisp!(format! "{} + {} = {}" 2 3 5);
    assert_eq!(s, "2 + 3 = 5");
}

#[test]
fn macro_assert_eq() {
    lisp!(assert_eq! 3 (+ 1 2));
}

#[test]
fn macro_vec() {
    // vec! via the `vec` keyword (preferred form)
    let v = lisp!(vec 1 2 3);
    assert_eq!(v, vec![1, 2, 3]);
}

#[test]
fn macro_bang_shorthand() {
    let s = lisp!(format! "hello {}" "world");
    assert_eq!(s, "hello world");
}

// =============================================================================
// 14. Modules & Imports
// =============================================================================

lisp!(mod inner_math
    (pub fn double ((x i32)) i32 (* x 2)));

#[test]
fn module_function() {
    assert_eq!(inner_math::double(5), 10);
}

// Type alias
lisp!(type IntPair = (i32, i32));

#[test]
fn type_alias() {
    let pair: IntPair = (1, 2);
    assert_eq!(pair.0 + pair.1, 3);
}

// =============================================================================
// 15. Unsafe
// =============================================================================

#[test]
fn unsafe_block() {
    let x = 42;
    let ptr = &x as *const i32;
    let val = lisp!(unsafe (rust { *ptr }));
    assert_eq!(val, 42);
}

#[test]
fn unsafe_fn_call() {
    let result = unsafe { unsafe_double(21) };
    assert_eq!(result, 42);
}

// =============================================================================
// 16. Rust Escape Hatch
// =============================================================================

#[test]
fn rust_brace_escape() {
    let result: i32 = lisp!(rust { 2 + 3 });
    assert_eq!(result, 5);
}

#[test]
fn rust_brace_complex() {
    let result: i32 = lisp!(rust {
        let x = 10;
        let y = 20;
        x + y
    });
    assert_eq!(result, 30);
}

#[test]
fn rust_stmt_escape() {
    lisp!(rust
        let x: i32 = 42
    );
    assert_eq!(x, 42);
}

// =============================================================================
// 17. Self Params in Methods
// =============================================================================

#[test]
fn self_param_ref() {
    let p = Point::new(3, 4);
    assert_eq!(p.describe(), "Point(3, 4)");
}

#[test]
fn self_param_owned() {
    let p = Point::new(1, 2);
    let q = Point::new(3, 4);
    let r = p + q; // uses owned self via Add
    assert_eq!(r.x, 4);
    assert_eq!(r.y, 6);
}

#[test]
fn self_param_mut() {
    lisp!(struct Accumulator ((total i32)));
    lisp!(impl Accumulator
        (fn add ((&mut self) (val i32))
            (+= self.total val)));
    let mut acc = Accumulator { total: 0 };
    acc.add(10);
    acc.add(20);
    assert_eq!(acc.total, 30);
}

// =============================================================================
// 18. Trait Implementations — Verify Behavior
// =============================================================================

#[test]
fn trait_display() {
    let p = Point::new(3, 7);
    assert_eq!(format!("{}", p), "(3, 7)");
}

#[test]
fn trait_add() {
    let a = Point::new(1, 2);
    let b = Point::new(3, 4);
    let c = a + b;
    assert_eq!(c.x, 4);
    assert_eq!(c.y, 6);
}

#[test]
fn trait_default() {
    let p: Point = Default::default();
    assert_eq!(p.x, 0);
    assert_eq!(p.y, 0);
}

#[test]
fn trait_clone() {
    let a = Point::new(5, 10);
    let b = a.clone();
    assert_eq!(b.x, 5);
    assert_eq!(b.y, 10);
}

#[test]
fn trait_partial_eq() {
    let a = Point::new(1, 2);
    let b = Point::new(1, 2);
    let c = Point::new(3, 4);
    assert!(a == b);
    assert!(a != c);
}

#[test]
fn trait_neg() {
    let p = Point::new(3, -4);
    let neg_p = -p;
    assert_eq!(neg_p.x, -3);
    assert_eq!(neg_p.y, 4);
}

#[test]
fn trait_drop() {
    use std::cell::Cell;
    thread_local! {
        static DROP_COUNT: Cell<u32> = Cell::new(0);
    }
    DROP_COUNT.with(|c| c.set(0));
    {
        let _guard = DropCounter { count: DROP_COUNT.with(|c| {
            // Need a static ref; use a leaked cell for testing
            // Instead, use the existing pattern from core_traits test
            unsafe {
                let ptr = c as *const Cell<u32> as *const Cell<u32>;
                &*ptr
            }
        })};
    }
    DROP_COUNT.with(|c| assert_eq!(c.get(), 1));
}

#[test]
fn trait_from() {
    let p: Point = Point::from((10, 20));
    assert_eq!(p.x, 10);
    assert_eq!(p.y, 20);
}

#[test]
fn trait_iterator() {
    let counter = Counter { current: 0, max: 5 };
    let sum: i32 = counter.sum();
    assert_eq!(sum, 10); // 0+1+2+3+4

    let collected: Vec<i32> = Counter { current: 0, max: 4 }.collect();
    assert_eq!(collected, vec![0, 1, 2, 3]);
}

#[test]
fn trait_has_area() {
    let circle = Shape::Circle(1.0);
    assert!((circle.area() - std::f64::consts::PI).abs() < 1e-10);
    let rect = Shape::Rect { w: 3.0, h: 4.0 };
    assert!((rect.area() - 12.0).abs() < 1e-10);
    assert!((Shape::Unit.area() - 0.0).abs() < 1e-10);
}

// =============================================================================
// 19. Generic Functions
// =============================================================================

#[test]
fn generic_fn_add() {
    assert_eq!(generic_add(3, 4), 7);
    assert_eq!(generic_add(1.5, 2.5), 4.0);
}

#[test]
fn generic_fn_where() {
    let s = debug_value(&42);
    assert_eq!(s, "42");
}

// =============================================================================
// 20. Enum Pattern Matching
// =============================================================================

#[test]
fn enum_construction_and_match() {
    let shapes = vec![
        Shape::Circle(5.0),
        Shape::Rect { w: 3.0, h: 4.0 },
        Shape::Unit,
    ];
    let mut areas = Vec::new();
    for s in &shapes {
        areas.push(s.area());
    }
    assert!((areas[0] - std::f64::consts::PI * 25.0).abs() < 1e-10);
    assert!((areas[1] - 12.0).abs() < 1e-10);
    assert!((areas[2] - 0.0).abs() < 1e-10);
}

#[test]
fn generic_enum() {
    let x: Maybe<i32> = Maybe::Just(42);
    let y: Maybe<i32> = Maybe::Nothing;
    match x {
        Maybe::Just(v) => assert_eq!(v, 42),
        Maybe::Nothing => panic!("expected Just"),
    }
    match y {
        Maybe::Just(_) => panic!("expected Nothing"),
        Maybe::Nothing => {} // ok
    }
}

// =============================================================================
// 21. Complex Real-World Example: FizzBuzz
// =============================================================================

#[test]
fn fizzbuzz() {
    let result = lisp!(block
        (let mut out (vec))
        (for i in (..= 1 20)
            (if (== (% i 15) 0)
                (out.push "FizzBuzz")
                (if (== (% i 3) 0)
                    (out.push "Fizz")
                    (if (== (% i 5) 0)
                        (out.push "Buzz")
                        (out.push "number")))))
        (val out));
    assert_eq!(result[0], "number");    // 1
    assert_eq!(result[2], "Fizz");      // 3
    assert_eq!(result[4], "Buzz");      // 5
    assert_eq!(result[14], "FizzBuzz"); // 15
    assert_eq!(result.len(), 20);
}

// =============================================================================
// 22. Complex Real-World Example: Iterative Fibonacci
// =============================================================================

lisp!(fn fib_iter ((n i32)) i32
    (let mut a 0)
    (let mut b 1)
    (for _i in (.. 0 n)
        (let temp b)
        (= b (+ a b))
        (= a temp))
    (+ a 0));

#[test]
fn fibonacci_sequence() {
    let fibs: Vec<i32> = lisp!(block
        (let mut results (vec))
        (for i in (.. 0 10)
            (results.push (fib_iter i)))
        (val results));
    assert_eq!(fibs, vec![0, 1, 1, 2, 3, 5, 8, 13, 21, 34]);
}

// =============================================================================
// 23. Struct with Methods — Stats Tracker
// =============================================================================

lisp!(struct Stats ((count i32) (sum f64)));

lisp!(impl Stats
    (fn new () Stats
        (new Stats (count 0) (sum 0.0)))
    (fn add ((&mut self) (val f64))
        (+= self.count 1)
        (+= self.sum val))
    (fn mean ((&self)) f64
        (if (== (self.count) 0)
            0.0
            (/ (self.sum) (rust { self.count as f64 })))));

#[test]
fn stats_tracker() {
    let mut stats = Stats::new();
    let values = lisp!(vec 10.0 20.0 30.0 40.0 50.0);
    for v in &values {
        stats.add(*v);
    }
    assert_eq!(stats.count, 5);
    assert!((stats.mean() - 30.0).abs() < 1e-10);
}

// =============================================================================
// 24. Attributes (derive)
// =============================================================================

#[test]
fn derived_traits() {
    let d = Direction::North;
    let d2 = d.clone();
    assert_eq!(d, d2);
    assert_eq!(format!("{:?}", d), "North");
}

// =============================================================================
// 25. Val form (identity wrapper)
// =============================================================================

#[test]
fn val_form() {
    let x = 42;
    assert_eq!(lisp!(val x), 42);
}

// =============================================================================
// 26. Extern function call
// =============================================================================

#[test]
fn extern_c_fn() {
    assert_eq!(extern_add(10, 20), 30);
}

// =============================================================================
// 27. Method Calls & Path-Qualified Calls
// =============================================================================

#[test]
fn method_call() {
    let p = Point::new(3, 4);
    let q = p.translate(1, 2);
    assert_eq!(q.x, 4);
    assert_eq!(q.y, 6);
}

#[test]
fn path_qualified_call() {
    let p = lisp!(Point::new 1 2);
    assert_eq!(p.x, 1);
    assert_eq!(p.y, 2);
}

#[test]
fn path_qualified_static_method() {
    let p = lisp!(Point::origin);
    assert_eq!(p.x, 0);
    assert_eq!(p.y, 0);
}

// =============================================================================
// 28. Bool literals & self field access
// =============================================================================

#[test]
fn bool_literals() {
    assert_eq!(lisp!(true), true);
    assert_eq!(lisp!(false), false);
}

// =============================================================================
// 29. Recursive factorial (proves recursion works)
// =============================================================================

#[test]
fn recursive_factorial() {
    assert_eq!(factorial(0), 1);
    assert_eq!(factorial(1), 1);
    assert_eq!(factorial(5), 120);
    assert_eq!(factorial(10), 3628800);
}

// =============================================================================
// 30. Container/generic impl
// =============================================================================

#[test]
fn generic_container() {
    let c = Container { val: 42 };
    assert_eq!(c.unwrap(), 42);
    assert_eq!(format!("{:?}", c), "Container { val: 42 }");
}
