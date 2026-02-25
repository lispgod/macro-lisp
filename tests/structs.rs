#![allow(dead_code)]
use macro_lisp::lisp;

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
