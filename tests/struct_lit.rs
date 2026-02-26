#![allow(dead_code)]
use macro_lisp::lisp;

// P16: struct construction with spread syntax and field shorthand

#[test]
fn struct_lit_spread() {
    #[derive(Debug, PartialEq)]
    struct Point { x: i32, y: i32 }
    let default = Point { x: 0, y: 0 };
    lisp!(let p (new Point (x 5) (.. default)));
    assert_eq!(p, Point { x: 5, y: 0 });
}

#[test]
fn struct_lit_shorthand() {
    #[derive(Debug, PartialEq)]
    struct Point { x: i32, y: i32 }
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

    let base = Config { width: 10, height: 20, depth: 30 };
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
