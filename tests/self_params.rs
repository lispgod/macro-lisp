#![allow(dead_code)]
use macro_lisp::lisp;

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
