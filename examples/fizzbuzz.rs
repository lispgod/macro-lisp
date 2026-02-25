// run command:
//    $ cargo run --example fizzbuzz

use macro_lisp::lisp;

lisp!(fn main() ()
    (for count in (.. 0 100)
        (let num (+ count 1))
        (if (== 0 (% num 3))
            (if (== 0 (% num 5))
                (macro! println "FizzBuzz")
                (macro! println "Fizz"))
            (if (== 0 (% num 5))
                (macro! println "Buzz")
                (macro! println "{}" num))))
);
