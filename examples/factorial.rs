// run command:
//    $ cargo run --example factorial

use macro_lisp::lisp;

lisp!(fn factorial ((n i32)) i32
  (if (<= n 1)
    1
    (* n (factorial (- n 1)))));

lisp!(fn main () ()
    (let num (factorial 10))
    (macro! println "10! = {}" num));
