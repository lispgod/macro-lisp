#![allow(unused_variables)]
#![allow(unused_mut)]
#![allow(dead_code)]

use macro_lisp::lisp;

// struct definitions
lisp!(struct Point
    ((x i32) (y i32))
);

lisp!(struct Color);

lisp!(struct Pair<T>
    ((first T) (second T))
);

lisp!(struct PubFields
    (pub (name String) (age i32))
);

lisp!(struct Mixed
    (pub (name String))
    ((secret i32))
);

#[test]
fn struct_basic() {
    let p = Point { x: 1, y: 2 };
    assert_eq!(p.x, 1);
    assert_eq!(p.y, 2);
}

#[test]
fn struct_unit() {
    let _c = Color;
}

#[test]
fn struct_generic() {
    let p: Pair<i32> = Pair {
        first: 10,
        second: 20,
    };
    assert_eq!(p.first, 10);
    assert_eq!(p.second, 20);
}

#[test]
fn struct_pub_fields() {
    let p = PubFields {
        name: "Alice".to_string(),
        age: 30,
    };
    assert_eq!(p.name, "Alice");
    assert_eq!(p.age, 30);
}

#[test]
fn struct_mixed_fields() {
    let m = Mixed {
        name: "Bob".to_string(),
        secret: 42,
    };
    assert_eq!(m.name, "Bob");
}

// enum definitions
lisp!(enum Direction { North, South, East, West });

#[test]
fn enum_basic() {
    let d = Direction::North;
    let result = match d {
        Direction::North => 1,
        Direction::South => 2,
        Direction::East => 3,
        Direction::West => 4,
    };
    assert_eq!(result, 1);
}

// impl blocks
lisp!(impl Point
    (fn origin () Point
        (new Point (x 0) (y 0)))
);

#[test]
fn impl_method() {
    let p = Point::origin();
    assert_eq!(p.x, 0);
    assert_eq!(p.y, 0);
}

// type alias
lisp!(type Integer = i32);

#[test]
fn type_alias() {
    let x: Integer = 42;
    assert_eq!(x, 42i32);
}

// new (struct construction)
#[test]
fn new_struct() {
    let p = lisp!(new Point (x 10) (y 20));
    assert_eq!(p.x, 10);
    assert_eq!(p.y, 20);
}

#[test]
fn new_struct_with_expr() {
    let p = lisp!(new Point (x (+ 1 2)) (y (* 3 4)));
    assert_eq!(p.x, 3);
    assert_eq!(p.y, 12);
}

// field access
#[test]
fn field_access() {
    let p = Point { x: 42, y: 99 };
    let x = lisp!(field p x);
    assert_eq!(x, 42);
}

// const
lisp!(const MAX_SIZE usize = 100);

#[test]
fn const_value() {
    assert_eq!(MAX_SIZE, 100);
}
