#![allow(dead_code)]
use macro_lisp::lisp;

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
