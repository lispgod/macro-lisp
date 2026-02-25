#![allow(dead_code)]
use macro_lisp::lisp;

// P13: Where clauses on all items

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
    struct Pair<T> { a: T, b: T }
    lisp!(impl<T> Pair<T> where (T: core::ops::Add<Output = T> + Copy)
        (fn sum ((&self)) T
            (+ (self.a) (self.b))));
    let p = Pair { a: 3, b: 4 };
    assert_eq!(p.sum(), 7);
}
