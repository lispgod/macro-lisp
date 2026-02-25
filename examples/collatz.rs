// run command:
//    $ cargo run --example collatz

use macro_lisp::lisp;

// Compute the length of the Collatz sequence starting at n
lisp!(fn collatz_len ((n i64)) i64
    (defvar x n)
    (defvar count 0)
    (while (> x 1)
        (if (== (% x 2) 0)
            (setf x (/ x 2))
            (setf x (+ (* x 3) 1)))
        (incf count))
    (+ count 0)
);

lisp!(fn main () ()
    (println "Collatz sequence lengths:")
    (dotimes (i 20)
        (let n (+ i 1))
        (let steps (collatz_len n))
        (println "  collatz({}) = {} steps" n steps))
);
