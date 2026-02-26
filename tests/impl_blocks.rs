#![allow(dead_code)]
use macro_lisp::lisp;

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
struct V2 { x: i32, y: i32 }

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
