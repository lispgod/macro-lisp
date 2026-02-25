#![allow(unused_variables)]
#![allow(unused_mut)]
#![allow(dead_code)]

use macro_lisp::lisp;

#[test]
fn fizzbuzz() {
    let result = lisp!(block
        (let mut out (vec))
        (for i in (.. 1 101)
            (if (== (% i 15) 0)
                (out.push "FizzBuzz")
                (if (== (% i 3) 0)
                    (out.push "Fizz")
                    (if (== (% i 5) 0)
                        (out.push "Buzz")
                        (out.push "number")))))
        (val out)
    );
    assert_eq!(result[0], "number");    // 1
    assert_eq!(result[2], "Fizz");      // 3
    assert_eq!(result[4], "Buzz");      // 5
    assert_eq!(result[14], "FizzBuzz"); // 15
    assert_eq!(result.len(), 100);
}

lisp!(fn fibonacci ((n i32)) i32
    (if (<= n 1)
        n
        (+ (fibonacci (- n 1)) (fibonacci (- n 2)))));

#[test]
fn fibonacci_sequence() {
    let fibs: Vec<i32> = lisp!(block
        (let mut results (vec))
        (for i in (.. 0 10)
            (results.push (fibonacci i)))
        (val results)
    );
    assert_eq!(fibs, vec![0, 1, 1, 2, 3, 5, 8, 13, 21, 34]);
}

lisp!(fn collatz_len ((n i64)) i64
    (let mut x n)
    (let mut count 0)
    (while (> x 1)
        (if (== (% x 2) 0)
            (= x (/ x 2))
            (= x (+ (* x 3) 1)))
        (+= count 1))
    (+ count 0)
);

#[test]
fn collatz_sequence_lengths() {
    assert_eq!(lisp!(collatz_len 1), 0);
    assert_eq!(lisp!(collatz_len 2), 1);
    assert_eq!(lisp!(collatz_len 3), 7);
    assert_eq!(lisp!(collatz_len 6), 8);
    assert_eq!(lisp!(collatz_len 27), 111);
}

lisp!(fn factorial ((n i32)) i32
    (if (<= n 1)
        1
        (* n (factorial (- n 1)))));

#[test]
fn factorial_comprehensive() {
    assert_eq!(lisp!(factorial 0), 1);
    assert_eq!(lisp!(factorial 1), 1);
    assert_eq!(lisp!(factorial 5), 120);
    assert_eq!(lisp!(factorial 10), 3628800);
}

lisp!(struct Vec2
    ((x f64) (y f64))
);

lisp!(impl Vec2
    (fn magnitude ((self Vec2)) f64
        (rust let sum_sq = self.x * self.x + self.y * self.y)
        (sum_sq.sqrt))
);

#[test]
fn struct_with_impl() {
    let v = lisp!(struct - lit Vec2 (x 3.0) (y 4.0));
    assert_eq!(v.x, 3.0);
    assert_eq!(v.y, 4.0);
    let mag = v.magnitude();
    assert!((mag - 5.0).abs() < 1e-10);
}

#[test]
fn variadic_arithmetic_chain() {
    // (+ 1 2 3 4 5) = 15
    assert_eq!(lisp!(+ 1 2 3 4 5), 15);
    // (* 1 2 3 4) = 24
    assert_eq!(lisp!(* 1 2 3 4), 24);
    // (- 100 10 20 30) = 40
    assert_eq!(lisp!(- 100 10 20 30), 40);
}

#[test]
fn nested_blocks_and_control() {
    let result = lisp!(block
        (let mut total 0)
        (for i in (.. 1 11)
            (if (== (% i 2) 0)
                (+= total i)))
        (val total)
    );
    // Sum of even numbers 2+4+6+8+10 = 30
    assert_eq!(result, 30);
}

#[test]
fn complex_closures() {
    let double = lisp!(fn ((x i32)) (* x 2));
    let add_ten = lisp!(fn ((x i32)) (+ x 10));

    let results: Vec<i32> = (1..=5).map(double).collect();
    assert_eq!(results, vec![2, 4, 6, 8, 10]);

    let results2: Vec<i32> = results.iter().map(|&x| add_ten(x)).collect();
    assert_eq!(results2, vec![12, 14, 16, 18, 20]);
}

#[test]
fn try_operator_in_fn() {
    fn parse_and_add(a: &str, b: &str) -> Result<i32, std::num::ParseIntError> {
        let pa: Result<i32, _> = a.parse();
        let pb: Result<i32, _> = b.parse();
        let x = lisp!(? pa);
        let y = lisp!(? pb);
        Ok(lisp!(+ x y))
    }
    assert_eq!(parse_and_add("3", "4").unwrap(), 7);
    assert!(parse_and_add("abc", "4").is_err());
}

#[test]
fn scoped_bindings_complex() {
    let result = lisp!(let ((a 10) (b 20) (c 30))
        (= a (+ a b))
        (= b (+ b c))
        (+ a b c)
    );
    // a = 10+20 = 30, b = 20+30 = 50, c = 30
    // result = 30 + 50 + 30 = 110
    assert_eq!(result, 110);
}

#[test]
fn format_and_print() {
    let s = lisp!(macro! format "{} + {} = {}" 2 3 5);
    assert_eq!(s, "2 + 3 = 5");
}

#[test]
fn rust_escape_hatch() {
    lisp!(rust
        let x: i32 = 42
    );
    assert_eq!(x, 42);
}

#[test]
fn range_inclusive_loop() {
    lisp!(let mut sum 0);
    lisp!(for i in (..= 1 10) (+= sum i));
    // 1+2+3+...+10 = 55
    assert_eq!(sum, 55);
}
