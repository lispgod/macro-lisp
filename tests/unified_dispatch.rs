/// Tests verifying unified dispatch through proc macros after the refactoring.
/// These validate that all modifier × return-type × generics combinations
/// work correctly through the consolidated lisp_fn! and lisp_struct! dispatch.

use macro_lisp::lisp;

// ── Named fn: all modifier combinations ─────────────────────

lisp!(fn bare_fn ((x i32)) i32 (+ x 1));
lisp!(pub fn pub_fn ((x i32)) i32 (+ x 2));
lisp!(const fn const_fn ((x i32)) i32 (+ x 3));
lisp!(pub const fn pub_const_fn ((x i32)) i32 (+ x 4));
lisp!(unsafe fn unsafe_fn ((x i32)) i32 (+ x 5));
lisp!(pub unsafe fn pub_unsafe_fn ((x i32)) i32 (+ x 6));
lisp!(async fn async_fn ((x i32)) i32 (+ x 7));
lisp!(pub async fn pub_async_fn ((x i32)) i32 (+ x 8));
lisp!(extern "C" fn extern_fn ((x i32)) i32 (+ x 9));
lisp!(pub extern "C" fn pub_extern_fn ((x i32)) i32 (+ x 10));

#[test]
fn all_fn_modifiers() {
    assert_eq!(bare_fn(0), 1);
    assert_eq!(pub_fn(0), 2);
    assert_eq!(const_fn(0), 3);
    assert_eq!(pub_const_fn(0), 4);
    assert_eq!(unsafe { unsafe_fn(0) }, 5);
    assert_eq!(unsafe { pub_unsafe_fn(0) }, 6);
    assert_eq!(extern_fn(0), 9);
    assert_eq!(pub_extern_fn(0), 10);
}

// ── Named fn: void (no return type) ─────────────────────────

lisp!(fn void_fn () (let _x 42));
lisp!(pub fn pub_void_fn () (let _x 42));

#[test]
fn void_fns() {
    void_fn();
    pub_void_fn();
}

// ── Named fn: () unit return type ───────────────────────────

lisp!(fn unit_return_fn () () (let _x 42));

#[test]
fn unit_return_type() {
    let result: () = unit_return_fn();
    assert_eq!(result, ());
}

// ── Named fn: with generics ────────────────────────────────

lisp!(fn generic_fn <T: std::fmt::Debug> ((x T)) (println! "{:?}" x));

#[test]
fn generics_through_dispatch() {
    generic_fn(42);
    generic_fn("hello");
}

// ── Struct: all forms ──────────────────────────────────────

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

// ── Struct: with generics ──────────────────────────────────

lisp!(struct GenericStruct <T> ((value T)));

#[test]
fn struct_generics() {
    let s = GenericStruct { value: 42 };
    assert_eq!(s.value, 42);
    let s = GenericStruct { value: "hello" };
    assert_eq!(s.value, "hello");
}

// ── Struct: with derive ────────────────────────────────────

lisp!(#[derive(Debug, Clone, PartialEq)] struct DerivedStruct ((x i32)));

#[test]
fn struct_derive() {
    let s = DerivedStruct { x: 42 };
    let s2 = s.clone();
    assert_eq!(s, s2);
    assert_eq!(format!("{:?}", s), "DerivedStruct { x: 42 }");
}

// ── Compound assignment: unified proc-macro dispatch ──────

#[test]
fn compound_assign_simple_ident() {
    let mut x: i32 = 10;
    lisp!(+= x 5);
    assert_eq!(x, 15);
    lisp!(-= x 3);
    assert_eq!(x, 12);
    lisp!(*= x 2);
    assert_eq!(x, 24);
    lisp!(/= x 4);
    assert_eq!(x, 6);
    lisp!(%= x 4);
    assert_eq!(x, 2);
}

#[test]
fn compound_assign_bitwise() {
    let mut x: u8 = 0xFF;
    lisp!(&= x 0x0F);
    assert_eq!(x, 0x0F);
    lisp!(|= x 0xF0);
    assert_eq!(x, 0xFF);
    lisp!(^= x 0x0F);
    assert_eq!(x, 0xF0);
    lisp!(<<= x 1);
    assert_eq!(x, 0xE0);
    lisp!(>>= x 4);
    assert_eq!(x, 0x0E);
}

// ── const/static: visibility without = separator ──────────

lisp!(pub const PUB_CONST_NO_EQ i32 100);
lisp!(pub static PUB_STATIC_NO_EQ i32 200);

#[test]
fn pub_const_static_without_eq() {
    assert_eq!(PUB_CONST_NO_EQ, 100);
    assert_eq!(PUB_STATIC_NO_EQ, 200);
}

// ── Variadic * and / ──────────────────────────────────────

#[test]
fn variadic_multiply() {
    let result: i32 = lisp!(* 2 3 4);
    assert_eq!(result, 24);
}

#[test]
fn variadic_divide() {
    let result: i32 = lisp!(/ 120 2 3);
    assert_eq!(result, 20);
}
