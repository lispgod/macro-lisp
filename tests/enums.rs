#![allow(dead_code)]
use macro_lisp::lisp;

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
