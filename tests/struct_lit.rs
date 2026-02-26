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
