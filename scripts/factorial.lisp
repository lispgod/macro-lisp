;; factorial.lisp — Recursive factorial
;; Run with: cargo run -p macro-lisp-cli -- run scripts/factorial.lisp

(fn factorial ((n i32)) i32
  (if (<= n 1)
    1
    (* n (factorial (- n 1)))))

(fn main () ()
    (for i in (..= 1 10)
        (println! "{}! = {}" i (factorial i))))
