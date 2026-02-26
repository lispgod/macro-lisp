#![allow(unused_variables)]
#![allow(unused_mut)]
#![allow(dead_code)]
#![allow(unused_unsafe)]

/// Tests for all new grammar specification forms.
/// Each test validates a specific form from the Complete Grammar — Step 1.
use macro_lisp::lisp;

// ── Unary Negation ──────────────────────────────────────────

#[test]
fn neg_integer() {
    let x = 5;
    let result: i32 = lisp!(neg x);
    assert_eq!(result, -5);
}

#[test]
fn neg_float() {
    let x = 3.14_f64;
    let result: f64 = lisp!(neg x);
    assert!((result - (-3.14)).abs() < 1e-10);
}

// ── Bitwise Operators ───────────────────────────────────────

#[test]
fn bitwise_and() {
    let result: u8 = lisp!(& 0b1100 0b1010);
    assert_eq!(result, 0b1000);
}

#[test]
fn bitwise_or() {
    let result: u8 = lisp!(| 0b1100 0b1010);
    assert_eq!(result, 0b1110);
}

#[test]
fn bitwise_xor() {
    let result: u8 = lisp!(^ 0b1100 0b1010);
    assert_eq!(result, 0b0110);
}

#[test]
fn bitwise_shl() {
    let result: u8 = lisp!(<< 1 4);
    assert_eq!(result, 16);
}

#[test]
fn bitwise_shr() {
    let result: u8 = lisp!(>> 16 2);
    assert_eq!(result, 4);
}

// ── Short-circuit Logical Operators ─────────────────────────

#[test]
fn logical_and_short_circuit() {
    let result: bool = lisp!(&& true false);
    assert_eq!(result, false);
}

#[test]
fn logical_and_true() {
    let result: bool = lisp!(&& true true);
    assert_eq!(result, true);
}

#[test]
fn logical_or_short_circuit() {
    let result: bool = lisp!(|| false true);
    assert_eq!(result, true);
}

#[test]
fn logical_or_false() {
    let result: bool = lisp!(|| false false);
    assert_eq!(result, false);
}

// ── Bitwise Compound Assignment ─────────────────────────────

#[test]
fn bitwise_and_assign() {
    let mut x: u8 = 0b1111;
    lisp!(&= x 0b1010);
    assert_eq!(x, 0b1010);
}

#[test]
fn bitwise_or_assign() {
    let mut x: u8 = 0b1010;
    lisp!(|= x 0b0101);
    assert_eq!(x, 0b1111);
}

#[test]
fn bitwise_xor_assign() {
    let mut x: u8 = 0b1100;
    lisp!(^= x 0b1010);
    assert_eq!(x, 0b0110);
}

#[test]
fn shl_assign() {
    let mut x: u8 = 1;
    lisp!(<<= x 3);
    assert_eq!(x, 8);
}

#[test]
fn shr_assign() {
    let mut x: u8 = 16;
    lisp!(>>= x 2);
    assert_eq!(x, 4);
}

// ── = Assignment ────────────────────────────────────────────

#[test]
fn eq_assign_simple() {
    let mut x = 0;
    lisp!(= x 10);
    assert_eq!(x, 10);
}

#[test]
fn eq_assign_expression() {
    let mut x = 0;
    lisp!(= x (+ 2 3));
    assert_eq!(x, 5);
}

// ── Enhanced Break ──────────────────────────────────────────

#[test]
fn break_with_value() {
    let result = lisp!(loop (break 42));
    assert_eq!(result, 42);
}

#[test]
fn break_with_label() {
    let mut count = 0;
    lisp!('outer loop
        (loop
            (+= count 1)
            (break 'outer)));
    assert_eq!(count, 1);
}

#[test]
fn break_with_label_and_value() {
    let result: i32 = lisp!('outer loop
        (loop
            (break 'outer 99)));
    assert_eq!(result, 99);
}

// ── Enhanced Continue ───────────────────────────────────────

#[test]
fn continue_with_label() {
    let mut sum = 0;
    let mut i = 0;
    lisp!('outer while (< i 5)
        (+= i 1)
        (if (== i 3)
            (continue 'outer))
        (+= sum i));
    assert_eq!(sum, 1 + 2 + 4 + 5);
}

// ── Range Operators (.. and ..=) ────────────────────────────

#[test]
fn range_dot_dot() {
    let mut sum = 0;
    for i in lisp!(.. 0 5) {
        sum += i;
    }
    assert_eq!(sum, 10); // 0+1+2+3+4
}

#[test]
fn range_dot_dot_inclusive() {
    let mut sum = 0;
    for i in lisp!(..= 0 5) {
        sum += i;
    }
    assert_eq!(sum, 15); // 0+1+2+3+4+5
}

#[test]
fn range_dot_dot_half_open() {
    let v = vec![10, 20, 30, 40, 50];
    let slice = &v[lisp!(..2)];
    assert_eq!(slice, &[30, 40, 50]);
}

#[test]
fn range_dot_dot_full() {
    let v = vec![10, 20, 30];
    let slice = &v[lisp!(..)];
    assert_eq!(slice, &[10, 20, 30]);
}

// ── . Field Access ──────────────────────────────────────────

#[test]
fn dot_field_access() {
    struct Point {
        x: i32,
        y: i32,
    }
    let p = Point { x: 10, y: 20 };
    let result: i32 = lisp!(. p x);
    assert_eq!(result, 10);
}

#[test]
fn dot_chained_field_access() {
    struct Inner {
        val: i32,
    }
    struct Outer {
        inner: Inner,
    }
    let o = Outer {
        inner: Inner { val: 42 },
    };
    let result: i32 = lisp!(. o inner val);
    assert_eq!(result, 42);
}

// ── Array ───────────────────────────────────────────────────

#[test]
fn array_basic() {
    let arr: [i32; 3] = lisp!(array 1 2 3);
    assert_eq!(arr, [1, 2, 3]);
}

#[test]
fn array_empty() {
    let arr: [i32; 0] = lisp!(array);
    assert_eq!(arr, []);
}

// ── Struct Construction ─────────────────────────────────────

#[test]
fn struct_lit_construction() {
    struct Point {
        x: i32,
        y: i32,
    }
    let p: Point = lisp!(new Point (x 10) (y 20));
    assert_eq!(p.x, 10);
    assert_eq!(p.y, 20);
}

// ── Single-element Tuple ────────────────────────────────────

#[test]
fn tuple_single_element() {
    let t: (i32,) = lisp!(tuple 5);
    assert_eq!(t, (5,));
}

#[test]
fn tuple_unit() {
    let _t: () = lisp!(tuple);
}

#[test]
fn tuple_multi() {
    let t: (i32, i32, i32) = lisp!(tuple 1 2 3);
    assert_eq!(t, (1, 2, 3));
}

// ── Labeled Block ───────────────────────────────────────────

#[test]
fn labeled_block() {
    let result: i32 = lisp!(block 'blk
        (break 'blk 42));
    assert_eq!(result, 42);
}

// ── Labeled Loops ───────────────────────────────────────────

#[test]
fn labeled_loop() {
    let mut count = 0;
    lisp!('outer loop
        (+= count 1)
        (if (>= count 3) (break 'outer)));
    assert_eq!(count, 3);
}

#[test]
fn labeled_while() {
    let mut sum = 0;
    let mut i = 0;
    lisp!('w while (< i 5)
        (+= i 1)
        (+= sum i));
    assert_eq!(sum, 15);
}

#[test]
fn labeled_for() {
    let mut total = 0;
    lisp!('f for i in (.. 0 3)
        (+= total i));
    assert_eq!(total, 3); // 0+1+2
}

// ── mod keyword ─────────────────────────────────────────────

lisp!(mod inner_mod
    (pub fn add_one ((x i32)) i32 (+ x 1)));

#[test]
fn mod_keyword() {
    assert_eq!(inner_mod::add_one(5), 6);
}

// ── macro! invocation ───────────────────────────────────────

#[test]
fn macro_assert_eq() {
    let x = 5;
    lisp!(macro ! assert_eq x 5);
}

#[test]
fn macro_format() {
    let s: String = lisp!(macro ! format "hello {}" "world");
    assert_eq!(s, "hello world");
}

// ── const fn ────────────────────────────────────────────────

lisp!(const fn const_add ((a i32) (b i32)) i32
    (+ a b));

#[test]
fn const_fn_basic() {
    const RESULT: i32 = const_add(2, 3);
    assert_eq!(RESULT, 5);
}

lisp!(pub const fn pub_const_double ((x i32)) i32
    (* x 2));

#[test]
fn pub_const_fn() {
    const RESULT: i32 = pub_const_double(7);
    assert_eq!(RESULT, 14);
}

// ── unsafe fn ───────────────────────────────────────────────

lisp!(unsafe fn unsafe_add ((a i32) (b i32)) i32
    (+ a b));

#[test]
fn unsafe_fn_basic() {
    let result = unsafe { unsafe_add(3, 4) };
    assert_eq!(result, 7);
}

lisp!(pub unsafe fn pub_unsafe_mul ((a i32) (b i32)) i32
    (* a b));

#[test]
fn pub_unsafe_fn() {
    let result = unsafe { pub_unsafe_mul(3, 4) };
    assert_eq!(result, 12);
}

// ── extern fn ───────────────────────────────────────────────

lisp!(extern "C" fn extern_c_add ((a i32) (b i32)) i32
    (+ a b));

#[test]
fn extern_c_fn_basic() {
    let result = extern_c_add(10, 20);
    assert_eq!(result, 30);
}

lisp!(pub extern "C" fn pub_extern_c_sub ((a i32) (b i32)) i32
    (- a b));

#[test]
fn pub_extern_c_fn() {
    let result = pub_extern_c_sub(20, 7);
    assert_eq!(result, 13);
}

// ── New const/static syntax (without =) ─────────────────────

lisp!(const MY_CONST i32 42);

#[test]
fn const_without_eq() {
    assert_eq!(MY_CONST, 42);
}

lisp!(static MY_STATIC i32 100);

#[test]
fn static_without_eq() {
    assert_eq!(MY_STATIC, 100);
}

lisp!(static mut MY_MUT_STATIC i32 0);

#[test]
fn static_mut_without_eq() {
    unsafe {
        MY_MUT_STATIC = 42;
        assert_eq!(MY_MUT_STATIC, 42);
    }
}

// ── Tuple struct ────────────────────────────────────────────

lisp!(struct Pair (i32 i32));

#[test]
fn tuple_struct_basic() {
    let p = Pair(1, 2);
    assert_eq!(p.0, 1);
    assert_eq!(p.1, 2);
}

lisp!(struct Wrapper (i32));

#[test]
fn tuple_struct_single() {
    let w = Wrapper(42);
    assert_eq!(w.0, 42);
}

// ── Nested labeled loops ────────────────────────────────────

#[test]
fn nested_labeled_loops() {
    let mut found = false;
    lisp!('outer for i in (.. 0 5)
        ('inner for j in (.. 0 5)
            (if (&& (== i 2) (== j 3))
                (block
                    (= found true)
                    (break 'outer)))));
    assert!(found);
}

// ── Combined features ───────────────────────────────────────

#[test]
fn combined_bitwise_and_shift() {
    // Extract bits 4-7 from a byte
    let byte: u8 = 0b1010_0110;
    let high_nibble: u8 = lisp!(& (>> byte 4) 0x0F);
    assert_eq!(high_nibble, 0b1010);
}

#[test]
fn combined_array_and_loop() {
    let arr: [i32; 5] = lisp!(array 10 20 30 40 50);
    let mut sum = 0;
    for val in arr.iter() {
        sum += val;
    }
    assert_eq!(sum, 150);
}

lisp!(const fn fib_const ((n u32)) u32
    (if (<= n 1) n (+ (fib_const (- n 1)) (fib_const (- n 2)))));

#[test]
fn const_fn_fibonacci() {
    const FIB_10: u32 = fib_const(10);
    assert_eq!(FIB_10, 55);
}

// ── module tests ────────────────────────────────────────────

// Test extern crate form
lisp!(extern crate std);

lisp!(mod module_test
    (fn do_nothing())

    (fn hello () ()
        (println! "Hello")
    )

    (fn add ((x i32) (y i32)) i32
        (+ x y)
    )

    (#[test] fn test_add () ()
        (let num (add 1 2))
        (assert_eq! 3 num)
    )
);

#[test]
fn extern_crate_form() {
    // extern crate std compiles at module level above
    assert!(true);
}

// ── async_unsafe tests ──────────────────────────────────────

#[test]
fn unsafe_block() {
    let x = lisp!(unsafe
        (let (x i32) 42)
        (+ x 0)
    );
    assert_eq!(x, 42);
}

// Note: async fn tests would require a runtime like tokio.
// We test that the macro at least expands to valid syntax for async fn.
// The await form requires an async context to actually run.

#[test]
fn unsafe_raw_pointer() {
    let x = 42i32;
    let p = &x as *const i32;
    let val = lisp!(unsafe (let v (deref p)) (+ v 0));
    assert_eq!(val, 42);
}

// ── visibility tests ────────────────────────────────────────

// P14: Visibility beyond pub — all item forms support $vis:vis uniformly.

lisp!(pub(crate) fn crate_internal ((x i32)) i32
    (+ x 1));

#[test]
fn pub_crate_fn() {
    assert_eq!(crate_internal(5), 6);
}

lisp!(pub(crate) const CRATE_CONST i32 = 42);

#[test]
fn pub_crate_const() {
    assert_eq!(CRATE_CONST, 42);
}

lisp!(pub(crate) type CrateInt = i32);

#[test]
fn pub_crate_type_alias() {
    let x: CrateInt = 10;
    assert_eq!(x, 10);
}

// Unified $vis:vis support: struct, enum, trait, static, mod

lisp!(pub(crate) struct VisStruct ((x i32)));

#[test]
fn pub_crate_struct() {
    let s = VisStruct { x: 5 };
    assert_eq!(s.x, 5);
}

lisp!(pub(crate) static VIS_STATIC i32 = 99);

#[test]
fn pub_crate_static() {
    assert_eq!(VIS_STATIC, 99);
}

lisp!(pub(crate) const fn crate_const_fn ((x i32)) i32
    (+ x 10));

#[test]
fn pub_crate_const_fn() {
    assert_eq!(crate_const_fn(5), 15);
}

mod visibility_test {
    use macro_lisp::lisp;

    lisp!(pub(super) fn pub_super_fn () i32 42);

    pub mod inner {
        use macro_lisp::lisp;
        lisp!(pub(super) fn inner_pub_super () i32 99);
    }

    pub fn get_inner_val() -> i32 {
        inner::inner_pub_super()
    }
}

#[test]
fn pub_super_fn() {
    assert_eq!(visibility_test::pub_super_fn(), 42);
    assert_eq!(visibility_test::get_inner_val(), 99);
}

// ── unified_dispatch tests ──────────────────────────────────

// Named fn: all modifier combinations

lisp!(fn bare_fn ((x i32)) i32 (+ x 1));
lisp!(pub fn pub_fn ((x i32)) i32 (+ x 2));
lisp!(const fn dispatch_const_fn ((x i32)) i32 (+ x 3));
lisp!(pub const fn dispatch_pub_const_fn ((x i32)) i32 (+ x 4));
lisp!(unsafe fn dispatch_unsafe_fn ((x i32)) i32 (+ x 5));
lisp!(pub unsafe fn dispatch_pub_unsafe_fn ((x i32)) i32 (+ x 6));
lisp!(async fn async_fn ((x i32)) i32 (+ x 7));
lisp!(pub async fn pub_async_fn ((x i32)) i32 (+ x 8));
lisp!(extern "C" fn dispatch_extern_fn ((x i32)) i32 (+ x 9));
lisp!(pub extern "C" fn dispatch_pub_extern_fn ((x i32)) i32 (+ x 10));

#[test]
fn all_fn_modifiers() {
    assert_eq!(bare_fn(0), 1);
    assert_eq!(pub_fn(0), 2);
    assert_eq!(dispatch_const_fn(0), 3);
    assert_eq!(dispatch_pub_const_fn(0), 4);
    assert_eq!(unsafe { dispatch_unsafe_fn(0) }, 5);
    assert_eq!(unsafe { dispatch_pub_unsafe_fn(0) }, 6);
    assert_eq!(dispatch_extern_fn(0), 9);
    assert_eq!(dispatch_pub_extern_fn(0), 10);
}

// Named fn: void (no return type)

lisp!(fn void_fn () (let _x 42));
lisp!(pub fn pub_void_fn () (let _x 42));

#[test]
fn void_fns() {
    void_fn();
    pub_void_fn();
}

// Named fn: () unit return type

lisp!(fn unit_return_fn () () (let _x 42));

#[test]
fn unit_return_type() {
    let result: () = unit_return_fn();
    assert_eq!(result, ());
}

// Named fn: with generics

lisp!(fn generic_fn <T: std::fmt::Debug> ((x T)) (println! "{:?}" x));

#[test]
fn generics_through_dispatch() {
    generic_fn(42);
    generic_fn("hello");
}

// Struct: all forms

lisp!(struct UnitStruct);
lisp!(pub struct PubUnitStruct);
lisp!(struct TupleStruct (i32 i32));
lisp!(pub struct PubTupleStruct (i32 String));
lisp!(struct NamedStruct ((x i32) (y i32)));
lisp!(pub struct PubNamedStruct ((x i32) (y i32)));
lisp!(struct PubFieldStruct (pub (x i32) (y i32)));
lisp!(struct MixedFieldStruct (pub (x i32)) ((y i32)));

#[test]
fn struct_unit() {
    let _s = UnitStruct;
    let _s = PubUnitStruct;
}

#[test]
fn struct_tuple() {
    let s = TupleStruct(1, 2);
    assert_eq!(s.0, 1);
    assert_eq!(s.1, 2);

    let s = PubTupleStruct(42, String::from("hello"));
    assert_eq!(s.0, 42);
}

#[test]
fn struct_named() {
    let s = NamedStruct { x: 1, y: 2 };
    assert_eq!(s.x, 1);
    assert_eq!(s.y, 2);

    let s = PubNamedStruct { x: 3, y: 4 };
    assert_eq!(s.x, 3);
}

#[test]
fn struct_pub_fields() {
    let s = PubFieldStruct { x: 10, y: 20 };
    assert_eq!(s.x, 10);
    assert_eq!(s.y, 20);
}

#[test]
fn struct_mixed_fields() {
    let s = MixedFieldStruct { x: 1, y: 2 };
    assert_eq!(s.x, 1);
}

// Struct: with generics

lisp!(struct GenericStruct <T> ((value T)));

#[test]
fn struct_generics() {
    let s = GenericStruct { value: 42 };
    assert_eq!(s.value, 42);
    let s = GenericStruct { value: "hello" };
    assert_eq!(s.value, "hello");
}

// Struct: with derive

lisp!(#[derive(Debug, Clone, PartialEq)] struct DerivedStruct ((x i32)));

#[test]
fn struct_derive() {
    let s = DerivedStruct { x: 42 };
    let s2 = s.clone();
    assert_eq!(s, s2);
    assert_eq!(format!("{:?}", s), "DerivedStruct { x: 42 }");
}
