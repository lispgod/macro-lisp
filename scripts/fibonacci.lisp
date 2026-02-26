;; fibonacci.lisp — Fibonacci sequence
;; Run with: cargo run -p macro-lisp-cli -- run scripts/fibonacci.lisp

(fn fib ((n i32)) i32
    (let mut a 0)
    (let mut b 1)
    (for _i in (.. 0 n)
        (let temp b)
        (= b (+ a b))
        (= a temp))
    (+ a 0))

(fn main () ()
    (println! "Fibonacci sequence:")
    (for i in (.. 0 20)
        (println! "  fib({}) = {}" i (fib i))))
