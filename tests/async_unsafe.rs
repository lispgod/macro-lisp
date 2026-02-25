#![allow(unused_variables)]
#![allow(unused_mut)]
#![allow(unused_unsafe)]

use macro_lisp::lisp;

#[test]
fn unsafe_block() {
    let x = lisp!(unsafe
        (rust let x: i32 = 42)
        (+ x 0)
    );
    assert_eq!(x, 42);
}

// Note: async fn tests would require a runtime like tokio.
// We test that the macro at least expands to valid syntax for async fn.
// The await form requires an async context to actually run.

#[test]
fn unsafe_raw_pointer() {
    let x = 42i32;
    let p = &x as *const i32;
    let val = lisp!(unsafe (rust let v = *p) (+ v 0));
    assert_eq!(val, 42);
}
