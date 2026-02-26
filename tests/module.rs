#![allow(unused_variables)]
#![allow(unused_mut)]
#![allow(dead_code)]

use macro_lisp::lisp;

// Test extern crate form
lisp!(extern crate std);

lisp!(mod module_test
    (fn do_nothing())

    (fn hello () ()
        (println! "Hello")
    )

    (fn add ((x i32) (y i32)) i32
        (+ x y)
    )

    (#[test] fn test_add () ()
        (let num (add 1 2))
        (assert_eq! 3 num)
    )
);

#[test]
fn extern_crate_form() {
    // extern crate std compiles at module level above
    assert!(true);
}
