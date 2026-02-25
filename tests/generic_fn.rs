use macro_lisp::lisp;

lisp!(fn max_of<T: PartialOrd> ((a T) (b T)) T
    (if (> a b) a b));

#[test]
fn generic_fn() {
    assert_eq!(max_of(3, 5), 5);
    assert_eq!(max_of(3.14, 2.71), 3.14);
}

lisp!(fn add_em<T> ((a T) (b T)) T
    where (T: core::ops::Add<Output = T>)
    (+ a b));

#[test]
fn generic_fn_where() {
    assert_eq!(add_em(3, 4), 7);
}
