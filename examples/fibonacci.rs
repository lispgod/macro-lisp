// run command:
//    $ cargo run --example fibonacci

use macro_lisp::lisp;

lisp!(fn fibonacci ((n i32)) i32
    (if (<= n 1)
        n
        (+ (fibonacci (- n 1)) (fibonacci (- n 2)))));

lisp!(fn main () ()
    (for i in (.. 0 10)
        (let n (+ i 1))
        (let result (fibonacci n))
        (macro! println "fib({}) = {}" n result)));
