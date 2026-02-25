// run command:
//    $ cargo run --example fibonacci

use macro_lisp::lisp;

lisp!(fn fibonacci ((n i32)) i32
    (if (<= n 1)
        n
        (+ (fibonacci (- n 1)) (fibonacci (- n 2)))));

lisp!(fn main () ()
    (dotimes (i 10)
        (let n (1+ i))
        (let result (fibonacci n))
        (println "fib({}) = {}" n result)));
