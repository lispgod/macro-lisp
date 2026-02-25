use macro_lisp::lisp;

#[test]
fn assign_field() {
    struct P { x: i32 }
    let mut p = P { x: 0 };
    lisp!(= p.x 5);
    assert_eq!(p.x, 5);
}

#[test]
fn assign_index() {
    let mut v = vec![1, 2, 3];
    lisp!(= v[0] 99);
    assert_eq!(v[0], 99);
}

#[test]
fn assign_deref() {
    let mut x = 5;
    let r = &mut x;
    lisp!(= (*r) 10);
    assert_eq!(x, 10);
}

#[test]
fn assign_nested() {
    struct Inner { val: i32 }
    struct Outer { inner: Inner }
    let mut o = Outer { inner: Inner { val: 0 } };
    lisp!(= o.inner.val 42);
    assert_eq!(o.inner.val, 42);
}

#[test]
fn compound_assign_field() {
    struct P { x: i32 }
    let mut p = P { x: 10 };
    lisp!(+= p.x 5);
    assert_eq!(p.x, 15);
}
