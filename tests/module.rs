#![allow(unused_variables)]
#![allow(unused_mut)]
#![allow(dead_code)]

use macro_lisp::lisp;

lisp!(module module_test
    (fn do_nothing())

    (fn hello () ()
        (println "Hello")
    )

    (fn add ((x i32) (y i32)) i32
        (+ x y)
    )

    (#[test] fn test_add () ()
        (let num (add 1 2))
        (assert eq 3 num)
    )
);
