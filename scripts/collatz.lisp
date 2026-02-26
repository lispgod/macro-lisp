;; collatz.lisp — Collatz conjecture step counter
;; Run with: cargo run -p macro-lisp-cli -- run scripts/collatz.lisp

(fn collatz_steps ((start i64)) i64
    (let mut n start)
    (let mut steps 0)
    (while (> n 1)
        (if (== (% n 2) 0)
            (= n (/ n 2))
            (= n (+ (* n 3) 1)))
        (+= steps 1))
    (+ steps 0))

(fn main () ()
    (println! "Collatz conjecture — steps to reach 1:")
    (for i in (..= 1 30)
        (let n (rust { i as i64 }))
        (println! "  collatz({}) = {} steps" i (collatz_steps n))))
