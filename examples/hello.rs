// run command:
//    $ cargo run --example hello

use macro_lisp::lisp;

lisp!(pub module module_test
    (pub fn hello () ()
        (println "Hello, macro-lisp!")
    )
);

lisp!(fn main () ()
    (module_test::hello)
);
