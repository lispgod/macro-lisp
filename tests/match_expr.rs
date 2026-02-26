use macro_lisp::lisp;

#[test]
fn match_bare_expr() {
    let x = 2;
    let r: &str = lisp!(match x
        (1 => "one")
        (2 => "two")
        (_ => "other"));
    assert_eq!(r, "two");
}

#[test]
fn match_with_lisp_body() {
    let x = 3;
    let r: i32 = lisp!(match x
        (1 => (+ 10 1))
        (2 => (+ 10 2))
        (_ => (+ 10 x)));
    assert_eq!(r, 13);
}

#[test]
fn match_multi_body() {
    let x = 2;
    let result = lisp!(match x
        (1 => (let a 10) (+ a 1))
        (2 => (let b 20) (+ b 2))
        (_ => 0));
    assert_eq!(result, 22);
}
