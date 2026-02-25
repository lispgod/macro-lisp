// run command:
//    $ cargo run --example fibonacci

use macro_lisp::lisp;

lisp!(defun fibonacci ((n i32)) i32
    (if (<= n 1)
        n
        (+ (fibonacci (- n 1)) (fibonacci (- n 2)))));

lisp!(defun main () ()
    (dotimes (i 10)
        (defconstant n (1+ i))
        (defconstant result (fibonacci n))
        (println "fib({}) = {}" n result)));
