#![allow(unused_variables)]
#![allow(unused_mut)]
#![allow(unused_assignments)]
#![allow(dead_code)]
#![allow(unreachable_code)]
//#![feature(trace_macros)]

#[cfg(test)]
mod tests {
    use macro_lisp::lisp;

    #[test]
    fn test_while_let() {
        lisp!(progn
            (defvar num Some(0))
            (while-let (Some(i) = num)
                (if (> i 9)
                    (setf num None)
                    (setf num Some(i + 1))
                )
            )
            (assert-eq None num)
        );
    }

    #[test]
    fn test_if_let() {
        lisp!(progn
            (let number Some(7))
            (if-let (Some(i) = number)
                // success let
                (assert-eq 7 i)
                // else
                (panic "fail if-let")
            )
        );
    }

    lisp!(defstruct Person
        (pub // public members
            (name String)
            (address String)
        )
        (    // non-public members
         (age i32)
        )
    );

    lisp!(defstruct Person2
        (pub (age i32))
    );

    lisp!(defstruct Point<T>
        ((x T) (y T))
    );

    #[test]
    fn test_struct() {
        lisp!(progn);
    }

    #[test]
    fn test_match() {
        lisp!(progn
            (let s "test")

            (let x (match s
                ("test" => (1))
                (_ =>  (-1))))
            (assert-eq 1 x)

            (match s
                ("hello" => (println "world"))
                (_ => (println "Hum?")))
        );
    }

    #[test]
    fn test_lambda() {
        lisp!(progn
            (let f
                (lambda ((x i32)) (1+ x)))
            (let x (f 5))
            (assert-eq 6 x)

            (let y ((lambda ((x i32)) (* x x)) 4))
            (assert-eq 16 y)
        );
    }

    #[test]
    fn test_loop_break() {
        lisp!(let ((x 0))
            (loop
                (loop
                    (break)
                    (incf x)
                )
                (incf x)
                (break)
            )
            (assert-eq 1 x)
        );
    }

    #[test]
    fn test_doiter() {
        let vec = lisp!(vec 1 2 3 4 5);
        lisp!(let ((x 0))
            (doiter (num vec)
                (setf x (+ x num)))
            (assert-eq 15 x)
        );
        lisp!(let ((x 0))
            (doiter (num (vec 1 2 3 4 5))
                (setf x (+ x num)))
            (assert-eq 15 x)
        );
    }

    #[test]
    fn test_do() {
        lisp!(progn
            (let num
                (do ((x 0 (1+ x))
                     (y 0 (+ y 2)))
                ((> x 5) y)))
            (assert-eq num 12)
        );
    }

    #[test]
    fn test_let() {
        lisp!(progn
            (let x 3)
            (let y 5)
            (let ((x 1)
                  (y 2))
                (incf x)
                (decf y)
                (assert-eq x 2)
                (assert-eq y 1))
            (assert-eq x 3)
            (assert-eq y 5)
        );
    }

    #[test]
    fn test_dotimes() {
        lisp!(progn
            (defvar x 0)
            (dotimes (y 5)
                (setf x (+ x y)))
            (assert-eq x 10)
        );
    }

    #[test]
    fn test_while() {
        lisp!(progn
            (defvar x 0)
            (while (< x 10)
                (incf x))
            (assert-eq x 10)
        );
    }

    #[test]
    fn test_when_unless() {
        lisp!(progn
            (defvar x 0)
            (when true
                (setf x 1))
            (when false
                (setf x 2))
            (assert-eq 1 x)

            (defvar y 0)
            (unless true
                (setf y 1))
            (unless false
                (setf y 2))
            (assert-eq y 2)
        );
    }

    #[test]
    fn test_progn() {
        lisp!(progn
            (let x 3)
            (let y 4)
            (defvar z (* x y))
            (assert-eq 12 z)
        );
    }

    #[test]
    fn test_if() {
        lisp!(if (eq 1 1) (println "equal"));
        lisp!(if (eq 2 2) (println "equal") (println "not equal"));
        let x = lisp!(if true (+ 1 1) (+ 2 2));
        assert_eq!(2, x);

        lisp!(if (<= 1 2) (println "True") (println "False"));
    }

    lisp!(fn hello () ()
        (println "Hello")
    );

    lisp!(fn add1 ((x i32)) i32
        (+ x 1)
    );

    lisp!(fn add ((x i32) (y i32)) i32
        (+ x y)
    );

    lisp!(fn do_nothing());

    #[test]
    fn test_fn() {
        let x = lisp!(add1 5);
        assert_eq!(6, x);

        let x = lisp!(add 3 4);
        assert_eq!(7, x);

        do_nothing();
    }

    #[test]
    fn test_macro_utils() {
        lisp!(print "hello, {}" "world");
    }

    #[test]
    fn test_let_binding() {
        lisp!(let x 3);
        assert_eq!(3, x);

        lisp!(let (y &str) "hello");
    }

    #[test]
    fn test_defvar() {
        lisp!(defvar x 0);
        assert_eq!(0, x);
        lisp!(setf x 1);
        assert_eq!(1, x);

        lisp!(defvar (x i64) 5);
        lisp!(defvar (s String) "test".to_owned());
    }

    #[test]
    fn test_vec() {
        let v = lisp!(vec 1 2 3);
        assert_eq!(vec![1, 2, 3], v);

        let empty: Vec<i32> = lisp!(vec);
        assert!(empty.is_empty());
    }

    #[test]
    fn test_tuple() {
        let t = lisp!(tuple 1 2 3);
        assert_eq!((1, 2, 3), t);

        let pair = lisp!(tuple 10 20);
        assert_eq!((10, 20), pair);
    }

    #[test]
    fn test_format() {
        let s = lisp!(format "{} + {} = {}" 1 2 3);
        assert_eq!("1 + 2 = 3", s);
    }

    #[test]
    fn test_arithmetic() {
        let a = lisp!(- 10 3);
        assert_eq!(7, a);

        let b = lisp!(* 4 5);
        assert_eq!(20, b);

        let c = lisp!(/ 20 4);
        assert_eq!(5, c);

        let d = lisp!(% 17 5);
        assert_eq!(2, d);

        let e = lisp!(+ 3 4);
        assert_eq!(7, e);
    }

    #[test]
    fn test_comparisons() {
        assert!(lisp!(!= 1 2));
        assert!(!lisp!(!= 1 1));

        assert!(lisp!(< 1 2));
        assert!(!lisp!(< 2 1));

        assert!(lisp!(> 2 1));
        assert!(!lisp!(> 1 2));

        assert!(lisp!(<= 1 2));
        assert!(lisp!(<= 2 2));
        assert!(!lisp!(<= 3 2));

        assert!(lisp!(>= 2 1));
        assert!(lisp!(>= 2 2));
        assert!(!lisp!(>= 1 2));

        assert!(lisp!(eq 5 5));
        assert!(!lisp!(eq 5 6));
    }

    #[test]
    fn test_not() {
        assert_eq!(true, lisp!(! false));
        assert_eq!(false, lisp!(! true));
    }

    #[test]
    fn test_len() {
        let v = vec![1, 2, 3, 4];
        assert_eq!(4, lisp!(len v));

        let s = "hello";
        assert_eq!(5, lisp!(len s));
    }

    #[test]
    fn test_one_plus_minus() {
        assert_eq!(6, lisp!(1+ 5));
        assert_eq!(4, lisp!(1- 5));
    }

    #[test]
    fn test_incf_decf() {
        lisp!(progn
            (defvar x 10)
            (incf x)
            (assert-eq 11 x)
            (decf x)
            (decf x)
            (assert-eq 9 x)
        );
    }

    #[test]
    fn test_rust_escape() {
        lisp!(rust
            let x: i32 = 42
        );
        assert_eq!(42, x);
    }

    #[test]
    fn test_lambda_move() {
        lisp!(progn
            (let val 42)
            (let f (lambda move () (+ val 0)))
            (let result (f))
            (assert-eq 42 result)
        );
    }

    lisp!(pub fn pub_add ((a i32) (b i32)) i32
        (+ a b)
    );

    #[test]
    fn test_pub_fn() {
        let result = lisp!(pub_add 10 20);
        assert_eq!(30, result);
    }

    #[test]
    fn test_generic_struct() {
        let p: Point<i32> = Point { x: 1, y: 2 };
        assert_eq!(1, p.x);
        assert_eq!(2, p.y);
    }

    #[test]
    fn test_loop_continue() {
        lisp!(progn
            (defvar sum 0)
            (dotimes (i 10)
                (if (== (% i 2) 0)
                    (continue))
                (setf sum (+ sum i)))
            (assert-eq 25 sum)
        );
    }

    lisp!(fn fibonacci ((n i32)) i32
        (if (<= n 1)
            n
            (+ (fibonacci (- n 1)) (fibonacci (- n 2)))));

    #[test]
    fn test_recursive_fn() {
        assert_eq!(0, lisp!(fibonacci 0));
        assert_eq!(1, lisp!(fibonacci 1));
        assert_eq!(1, lisp!(fibonacci 2));
        assert_eq!(55, lisp!(fibonacci 10));
    }

    #[test]
    fn test_nested_if() {
        lisp!(progn
            (let x 15)
            (let result
                (if (== (% x 15) 0)
                    (format "FizzBuzz")
                    (if (== (% x 3) 0)
                        (format "Fizz")
                        (if (== (% x 5) 0)
                            (format "Buzz")
                            (format "{}" x)))))
            (assert-eq "FizzBuzz" result)
        );
    }

    #[test]
    fn test_let_with_expr() {
        lisp!(progn
            (let x (+ 3 4))
            (assert-eq 7 x)

            (let (y i64) (+ 10 20))
            (assert-eq 30 y)
        );
    }

    #[test]
    fn test_setf_with_expr() {
        lisp!(progn
            (defvar x 0)
            (setf x (+ 3 4))
            (assert-eq 7 x)
        );
    }

    #[test]
    fn test_defvar_with_expr() {
        lisp!(progn
            (defvar x (+ 1 2))
            (assert-eq 3 x)

            (defvar (y i32) (* 3 4))
            (assert-eq 12 y)
        );
    }
}
