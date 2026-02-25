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
