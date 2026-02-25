// run command:
//    $ cargo run --example collatz

use macro_lisp::lisp;

// Compute the length of the Collatz sequence starting at n
lisp!(fn collatz_len ((n i64)) i64
    (let mut x n)
    (let mut count 0)
    (while (> x 1)
        (if (== (% x 2) 0)
            (set x (/ x 2))
            (set x (+ (* x 3) 1)))
        (+= count 1))
    (+ count 0)
);

lisp!(fn main () ()
    (println "Collatz sequence lengths:")
    (for i in (range 0 20)
        (let n (+ i 1))
        (let steps (collatz_len n))
        (println "  collatz({}) = {} steps" n steps))
);
