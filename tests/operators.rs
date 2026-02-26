#![allow(unused_variables)]
#![allow(unused_mut)]

use macro_lisp::lisp;

// Binary arithmetic
#[test]
fn add_binary() {
    assert_eq!(lisp!(+ 3 4), 7);
}

#[test]
fn sub_binary() {
    assert_eq!(lisp!(- 10 3), 7);
}

#[test]
fn mul_binary() {
    assert_eq!(lisp!(* 4 5), 20);
}

#[test]
fn div_binary() {
    assert_eq!(lisp!(/ 20 4), 5);
}

#[test]
fn rem_binary() {
    assert_eq!(lisp!(% 17 5), 2);
}

// Variadic arithmetic
#[test]
fn add_variadic_3() {
    assert_eq!(lisp!(+ 1 2 3), 6);
}

#[test]
fn add_variadic_4() {
    assert_eq!(lisp!(+ 1 2 3 4), 10);
}

#[test]
fn add_variadic_5() {
    assert_eq!(lisp!(+ 1 2 3 4 5), 15);
}

#[test]
fn sub_variadic() {
    assert_eq!(lisp!(- 100 10 20 30), 40);
}

#[test]
fn mul_variadic() {
    assert_eq!(lisp!(* 2 3 4), 24);
}

#[test]
fn div_variadic() {
    assert_eq!(lisp!(/ 120 2 3), 20);
}

// Comparison
#[test]
fn eq_op() {
    assert!(lisp!(== 5 5));
    assert!(!lisp!(== 5 6));
}

#[test]
fn ne_op() {
    assert!(lisp!(!= 1 2));
    assert!(!lisp!(!= 1 1));
}

#[test]
fn lt_op() {
    assert!(lisp!(< 1 2));
    assert!(!lisp!(< 2 1));
    assert!(!lisp!(< 1 1));
}

#[test]
fn gt_op() {
    assert!(lisp!(> 2 1));
    assert!(!lisp!(> 1 2));
    assert!(!lisp!(> 1 1));
}

#[test]
fn le_op() {
    assert!(lisp!(<= 1 2));
    assert!(lisp!(<= 2 2));
    assert!(!lisp!(<= 3 2));
}

#[test]
fn ge_op() {
    assert!(lisp!(>= 2 1));
    assert!(lisp!(>= 2 2));
    assert!(!lisp!(>= 1 2));
}

// Logical
#[test]
fn and_binary() {
    assert!(lisp!(&& true true));
    assert!(!lisp!(&& true false));
    assert!(!lisp!(&& false true));
    assert!(!lisp!(&& false false));
}

#[test]
fn or_binary() {
    assert!(lisp!(|| true true));
    assert!(lisp!(|| true false));
    assert!(lisp!(|| false true));
    assert!(!lisp!(|| false false));
}

#[test]
fn and_variadic() {
    assert!(lisp!(&& (&& true true) true));
    assert!(!lisp!(&& (&& true true) false));
}

#[test]
fn or_variadic() {
    assert!(lisp!(|| (|| false false) true));
    assert!(!lisp!(|| (|| false false) false));
}

#[test]
fn not_op() {
    assert_eq!(true, lisp!(!false));
    assert_eq!(false, lisp!(!true));
}

// Compound assignment
#[test]
fn add_assign() {
    lisp!(let mut x 10);
    lisp!(+= x 5);
    assert_eq!(x, 15);
}

#[test]
fn sub_assign() {
    lisp!(let mut x 10);
    lisp!(-= x 3);
    assert_eq!(x, 7);
}

#[test]
fn mul_assign() {
    lisp!(let mut x 5);
    lisp!(*= x 3);
    assert_eq!(x, 15);
}

#[test]
fn div_assign() {
    lisp!(let mut x 20);
    lisp!(/= x 4);
    assert_eq!(x, 5);
}

#[test]
fn rem_assign() {
    lisp!(let mut x 17);
    lisp!(%= x 5);
    assert_eq!(x, 2);
}

// Nested expressions in arithmetic
#[test]
fn nested_arithmetic() {
    let x = lisp!(+ (* 3 4) (- 10 5));
    assert_eq!(x, 17);
}

#[test]
fn deeply_nested() {
    let x = lisp!(+ (+ 1 2) (+ 3 4));
    assert_eq!(x, 10);
}

#[test]
fn variadic_bitwise() {
    let result: u8 = lisp!(& 0xFF 0x0F 0x03);
    assert_eq!(result, 0x03);

    let result2: u8 = lisp!(| 0x01 0x02 0x04);
    assert_eq!(result2, 0x07);

    let result3: u8 = lisp!(^ 0xFF 0x0F 0x01);
    assert_eq!(result3, 0xF1);
}
