#![allow(unused_variables)]
#![allow(unused_mut)]
#![allow(unused_assignments)]
#![allow(dead_code)]
#![allow(unreachable_code)]

#[cfg(test)]
mod tests {
    use macro_lisp::lisp;

    #[test]
    fn test_while_let() {
        lisp!(block
            (let mut num Some(0))
            (while let (Some(i) = num)
                (if (> i 9)
                    (= num None)
                    (= num Some(i + 1))
                )
            )
            (macro! assert_eq None num)
        );
    }

    #[test]
    fn test_if_let() {
        lisp!(block
            (let number Some(7))
            (if let (Some(i) = number)
                // success let
                (macro! assert_eq 7 i)
                // else
                (panic "fail if-let")
            )
        );
    }

    lisp!(struct Person
        (pub // public members
            (name String)
            (address String)
        )
        (    // non-public members
         (age i32)
        )
    );

    lisp!(struct Person2
        (pub (age i32))
    );

    lisp!(struct Point<T>
        ((x T) (y T))
    );

    #[test]
    fn test_struct() {
        lisp!(block);
    }

    #[test]
    fn test_match() {
        lisp!(block
            (let s "test")

            (let x (match s
                ("test" => (1))
                (_ =>  (-1))))
            (macro! assert_eq 1 x)

            (match s
                ("hello" => (macro! println "world"))
                (_ => (macro! println "Hum?")))
        );
    }

    #[test]
    fn test_closure() {
        lisp!(block
            (let f
                (fn ((x i32)) (+ x 1)))
            (let x (f 5))
            (macro! assert_eq 6 x)
        );
    }

    #[test]
    fn test_loop_break() {
        lisp!(let ((x 0))
            (loop
                (loop
                    (break)
                    (+= x 1)
                )
                (+= x 1)
                (break)
            )
            (macro! assert_eq 1 x)
        );
    }

    #[test]
    fn test_for_in() {
        let vec = lisp!(vec 1 2 3 4 5);
        lisp!(let ((x 0))
            (for num in vec
                (= x (+ x num)))
            (macro! assert_eq 15 x)
        );
        lisp!(let ((x 0))
            (for num in (vec 1 2 3 4 5)
                (= x (+ x num)))
            (macro! assert_eq 15 x)
        );
    }

    #[test]
    fn test_do_with_while() {
        lisp!(block
            (let mut x 0)
            (let mut y 0)
            (while (! (> x 5))
                (+= x 1)
                (+= y 2))
            (let num y)
            (macro! assert_eq num 12)
        );
    }

    #[test]
    fn test_let() {
        lisp!(block
            (let x 3)
            (let y 5)
            (let ((x 1)
                  (y 2))
                (+= x 1)
                (-= y 1)
                (macro! assert_eq x 2)
                (macro! assert_eq y 1))
            (macro! assert_eq x 3)
            (macro! assert_eq y 5)
        );
    }

    #[test]
    fn test_for_range() {
        lisp!(block
            (let mut x 0)
            (for y in (.. 0 5)
                (= x (+ x y)))
            (macro! assert_eq x 10)
        );
    }

    #[test]
    fn test_while() {
        lisp!(block
            (let mut x 0)
            (while (< x 10)
                (+= x 1))
            (macro! assert_eq x 10)
        );
    }

    #[test]
    fn test_when_unless() {
        lisp!(block
            (let mut x 0)
            (if true
                (= x 1))
            (if false
                (= x 2))
            (macro! assert_eq 1 x)

            (let mut y 0)
            (if (! true)
                (= y 1))
            (if (! false)
                (= y 2))
            (macro! assert_eq y 2)
        );
    }

    #[test]
    fn test_block() {
        lisp!(block
            (let x 3)
            (let y 4)
            (let mut z (* x y))
            (macro! assert_eq 12 z)
        );
    }

    #[test]
    fn test_if() {
        lisp!(if (== 1 1) (macro! println "equal"));
        lisp!(if (== 2 2) (macro! println "equal") (macro! println "not equal"));
        let x = lisp!(if true (+ 1 1) (+ 2 2));
        assert_eq!(2, x);

        lisp!(if (<= 1 2) (macro! println "True") (macro! println "False"));
    }

    lisp!(fn hello () ()
        (macro! println "Hello")
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
        lisp!(macro! print "hello, {}" "world");
    }

    #[test]
    fn test_let_binding() {
        lisp!(let x 3);
        assert_eq!(3, x);

        lisp!(let (y &str) "hello");
    }

    #[test]
    fn test_let_mut() {
        lisp!(let mut x 0);
        assert_eq!(0, x);
        lisp!(= x 1);
        assert_eq!(1, x);

        lisp!(let mut (x i64) 5);
        lisp!(let mut (s String) "test".to_owned());
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
        let s = lisp!(macro! format "{} + {} = {}" 1 2 3);
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

        assert!(lisp!(== 5 5));
        assert!(!lisp!(== 5 6));
    }

    #[test]
    fn test_not() {
        assert_eq!(true, lisp!(! false));
        assert_eq!(false, lisp!(! true));
    }

    #[test]
    fn test_len() {
        let v = vec![1, 2, 3, 4];
        assert_eq!(4, lisp!(v.len));

        let s = "hello";
        assert_eq!(5, lisp!(s.len));
    }

    #[test]
    fn test_compound_assignment() {
        lisp!(block
            (let mut x 10)
            (+= x 1)
            (macro! assert_eq 11 x)
            (-= x 1)
            (-= x 1)
            (macro! assert_eq 9 x)
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
    fn test_closure_move() {
        lisp!(block
            (let val 42)
            (let f (fn move () (+ val 0)))
            (let result (f))
            (macro! assert_eq 42 result)
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
        lisp!(block
            (let mut sum 0)
            (for i in (.. 0 10)
                (if (== (% i 2) 0)
                    (continue))
                (= sum (+ sum i)))
            (macro! assert_eq 25 sum)
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
        lisp!(block
            (let x 15)
            (let result
                (if (== (% x 15) 0)
                    (macro! format "FizzBuzz")
                    (if (== (% x 3) 0)
                        (macro! format "Fizz")
                        (if (== (% x 5) 0)
                            (macro! format "Buzz")
                            (macro! format "{}" x)))))
            (macro! assert_eq "FizzBuzz" result)
        );
    }

    #[test]
    fn test_let_with_expr() {
        lisp!(block
            (let x (+ 3 4))
            (macro! assert_eq 7 x)

            (let (y i64) (+ 10 20))
            (macro! assert_eq 30 y)
        );
    }

    #[test]
    fn test_set_with_expr() {
        lisp!(block
            (let mut x 0)
            (= x (+ 3 4))
            (macro! assert_eq 7 x)
        );
    }

    #[test]
    fn test_let_mut_with_expr() {
        lisp!(block
            (let mut x (+ 1 2))
            (macro! assert_eq 3 x)

            (let mut (y i32) (* 3 4))
            (macro! assert_eq 12 y)
        );
    }
}
