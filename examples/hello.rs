// run command:
//    $ cargo run --example hello

use macro_lisp::lisp;

lisp!(pub mod module_test
    (pub fn hello () ()
        (macro! println "Hello, macro-lisp!")
    )
);

lisp!(fn main () ()
    (module_test::hello)
);
