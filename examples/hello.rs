// run command:
//    $ cargo run --example hello

use macro_lisp::lisp;

lisp!(pub module module_test
    (pub defun hello () ()
        (println "Hello, macro-lisp!")
    )
);

lisp!(defun main () ()
    (module_test::hello)
);
