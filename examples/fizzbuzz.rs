// run command:
//    $ cargo run --example fizzbuzz

use macro_lisp::lisp;

lisp!(fn main() ()
    (dotimes (count 100)
        (let num (1+ count))
        (if (== 0 (% num 3))
            (if (== 0 (% num 5))
                (println "FizzBuzz")
                (println "Fizz"))
            (if (== 0 (% num 5))
                (println "Buzz")
                (println "{}" num))))
);
