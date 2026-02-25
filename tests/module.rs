#![allow(unused_variables)]
#![allow(unused_mut)]
#![allow(dead_code)]

use macro_lisp::lisp;

lisp!(mod module_test
    (fn do_nothing())

    (fn hello () ()
        (macro! println "Hello")
    )

    (fn add ((x i32) (y i32)) i32
        (+ x y)
    )

    (#[test] fn test_add () ()
        (let num (add 1 2))
        (macro! assert_eq 3 num)
    )
);
