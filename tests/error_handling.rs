#![allow(unused_variables)]
#![allow(unused_mut)]
#![allow(dead_code)]

use macro_lisp::lisp;

#[test]
fn return_from_fn() {
    lisp!(fn early_return ((x i32)) i32
        (if (< x 0) (return 0))
        (* x x)
    );
    assert_eq!(lisp!(early_return 5), 25);
    let neg = -3;
    assert_eq!(lisp!(early_return neg), 0);
}

#[test]
fn return_with_expression() {
    lisp!(fn compute ((x i32)) i32
        (return (+ x 10))
    );
    assert_eq!(lisp!(compute 5), 15);
}

#[test]
fn panic_test() {
    let result = std::panic::catch_unwind(|| {
        lisp!(panic "test panic");
    });
    assert!(result.is_err());
}

#[test]
fn assert_eq_test() {
    lisp!(macro! assert_eq 5 5);
    lisp!(macro! assert_eq "hello" "hello");
}

#[test]
fn assert_basic() {
    lisp!(macro! assert true "should be true");
}

#[test]
fn try_operator() {
    fn parse_int(s: &str) -> Result<i32, std::num::ParseIntError> {
        let parsed: Result<i32, _> = s.parse();
        let n = lisp!(? parsed);
        Ok(n + 1)
    }
    assert_eq!(parse_int("42").unwrap(), 43);
    assert!(parse_int("abc").is_err());
}
