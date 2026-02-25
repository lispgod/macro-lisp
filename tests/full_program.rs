#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_mut)]
#![allow(unused_assignments)]

use macro_lisp::lisp;

// =============================================================================
// Part 1: Token types for a simple expression language
// =============================================================================

lisp!(#[derive(Debug, Clone, PartialEq)]
 enum Token
    (Num f64)
    (Plus)
    (Minus)
    (Star)
    (Slash));

// =============================================================================
// Part 2: Expression AST — recursive via Box
// =============================================================================

lisp!(#[derive(Debug, Clone, PartialEq)]
 enum Expr
    (Literal f64)
    (Neg ((inner Box<Expr>)))
    (BinOp ((op BinOpKind) (lhs Box<Expr>) (rhs Box<Expr>))));

lisp!(#[derive(Debug, Clone, Copy, PartialEq)]
 enum BinOpKind
    (Add)
    (Sub)
    (Mul)
    (Div));

// =============================================================================
// Part 3: Evaluator struct
// =============================================================================

lisp!(struct Evaluator
    ((call_count i32))
);

lisp!(impl Evaluator
    (fn new () Evaluator
        (new Evaluator (call_count 0)))

    (fn eval ((&mut self) (expr &Expr)) f64
        (+= self.call_count 1)
        (rust {
            match expr {
                Expr::Literal(v) => *v,
                Expr::Neg { inner } => {
                    let val = self.eval(inner);
                    -val
                }
                Expr::BinOp { op, lhs, rhs } => {
                    let left = self.eval(lhs);
                    let right = self.eval(rhs);
                    match op {
                        BinOpKind::Add => left + right,
                        BinOpKind::Sub => left - right,
                        BinOpKind::Mul => left * right,
                        BinOpKind::Div => left / right,
                    }
                }
            }
        }))

    (fn get_call_count ((&self)) i32
        (self.call_count))
);

// =============================================================================
// Part 4: Display trait implementation for BinOpKind
// =============================================================================

lisp!(impl core::fmt::Display for BinOpKind
    (fn fmt ((self &Self) (f &mut core::fmt::Formatter)) core::fmt::Result
        (match self
            (BinOpKind::Add => (write! f "+"))
            (BinOpKind::Sub => (write! f "-"))
            (BinOpKind::Mul => (write! f "*"))
            (BinOpKind::Div => (write! f "/")))));

// =============================================================================
// Part 5: Display trait implementation for Expr
// =============================================================================

lisp!(impl core::fmt::Display for Expr
    (fn fmt ((self &Self) (f &mut core::fmt::Formatter)) core::fmt::Result
        (match self
            (Expr::Literal(v) => (write! f "{}" v))
            (Expr::Neg { inner } => (write! f "(-{})" inner))
            (Expr::BinOp { op, lhs, rhs } => (write! f "({} {} {})" lhs op rhs)))));

// =============================================================================
// Part 6: Helper functions to build AST nodes
// =============================================================================

lisp!(fn make_lit ((v f64)) Expr
    (Expr::Literal v)
);

lisp!(fn make_neg ((e Expr)) Expr
    (rust { Expr::Neg { inner: Box::new(e) } })
);

lisp!(fn make_binop ((op BinOpKind) (lhs Expr) (rhs Expr)) Expr
    (rust { Expr::BinOp { op, lhs: Box::new(lhs), rhs: Box::new(rhs) } })
);

// =============================================================================
// Part 7: Tokenizer function (uses rust escape for complex iterator logic)
// =============================================================================

type TokenVec = Vec<Token>;

lisp!(fn tokenize ((input &str)) TokenVec
    (rust {
        let mut tokens = Vec::new();
        let mut chars = input.chars().peekable();
        while let Some(&ch) = chars.peek() {
            match ch {
                ' ' => { chars.next(); }
                '+' => { tokens.push(Token::Plus); chars.next(); }
                '-' => { tokens.push(Token::Minus); chars.next(); }
                '*' => { tokens.push(Token::Star); chars.next(); }
                '/' => { tokens.push(Token::Slash); chars.next(); }
                _ => {
                    let mut num_str = String::new();
                    while let Some(&c) = chars.peek() {
                        if c.is_ascii_digit() || c == '.' {
                            num_str.push(c);
                            chars.next();
                        } else {
                            break;
                        }
                    }
                    if let Ok(n) = num_str.parse::<f64>() {
                        tokens.push(Token::Num(n));
                    }
                }
            }
        }
        tokens
    })
);

// =============================================================================
// Part 8: Classify token helper
// =============================================================================

type OptBinOp = Option<BinOpKind>;

lisp!(fn token_to_op ((tok &Token)) OptBinOp
    (rust {
        match tok {
            Token::Plus => Some(BinOpKind::Add),
            Token::Minus => Some(BinOpKind::Sub),
            Token::Star => Some(BinOpKind::Mul),
            Token::Slash => Some(BinOpKind::Div),
            _ => None,
        }
    })
);

// =============================================================================
// Part 9: Sum numbers with a for loop and while loop
// =============================================================================

lisp!(fn sum_range ((start i32) (end i32)) i32
    (let mut total 0)
    (for i in (.. start end)
        (+= total i))
    (+ total 0)
);

lisp!(fn sum_while ((n i32)) i32
    (let mut total 0)
    (let mut i 1)
    (while (<= i n)
        (+= total i)
        (+= i 1))
    (+ total 0)
);

// =============================================================================
// Part 10: Classify numbers — uses if/else chains, logical ops, comparisons
// =============================================================================

type StaticStr = &'static str;

lisp!(fn classify ((n i32)) StaticStr
    (if (< n 0)
        "negative"
        (if (== n 0)
            "zero"
            (if (&& (> n 0) (< n 10))
                "small"
                (if (&& (>= n 10) (< n 100))
                    "medium"
                    "large"))))
);

// =============================================================================
// Part 11: Collatz steps — while loop with compound assignment
// =============================================================================

lisp!(fn collatz_steps ((start i64)) i64
    (let mut n start)
    (let mut steps 0)
    (while (> n 1)
        (if (== (% n 2) 0)
            (= n (/ n 2))
            (= n (+ (* n 3) 1)))
        (+= steps 1))
    (+ steps 0)
);

// =============================================================================
// Part 12: Factorial iterative
// =============================================================================

lisp!(fn factorial_iter ((n i32)) i32
    (let mut result 1)
    (for i in (..= 1 n)
        (*= result i))
    (+ result 0)
);

// =============================================================================
// Part 13: Fibonacci using tuple (pair) tracking
// =============================================================================

lisp!(fn fib ((n i32)) i32
    (let mut a 0)
    (let mut b 1)
    (for _i in (.. 0 n)
        (let temp b)
        (= b (+ a b))
        (= a temp))
    (+ a 0)
);

// =============================================================================
// Part 14: Stats struct with methods — demonstrates &self, &mut self
// =============================================================================

lisp!(struct Stats
    ((count i32) (sum f64) (min f64) (max f64))
);

lisp!(impl Stats
    (fn new () Stats
        (new Stats (count 0) (sum 0.0) (min f64::INFINITY) (max f64::NEG_INFINITY)))

    (fn add ((&mut self) (val f64))
        (+= self.count 1)
        (+= self.sum val)
        (if (< val (self.min))
            (= self.min val))
        (if (> val (self.max))
            (= self.max val)))

    (fn mean ((&self)) f64
        (if (== (self.count) 0)
            0.0
            (/ (self.sum) (rust { self.count as f64 }))))

    (fn range ((&self)) f64
        (if (== (self.count) 0)
            0.0
            (- (self.max) (self.min))))
);

lisp!(impl core::fmt::Display for Stats
    (fn fmt ((self &Self) (f &mut core::fmt::Formatter)) core::fmt::Result
        (rust { write!(f, "Stats(n={}, mean={:.2})", self.count, self.mean()) })));

// =============================================================================
// Part 15: Simple stack (Vec wrapper)
// =============================================================================

type IntVec = Vec<i32>;

lisp!(struct Stack
    ((data IntVec))
);

type OptInt = Option<i32>;

lisp!(impl Stack
    (fn new () Stack
        (rust { Stack { data: Vec::new() } }))

    (fn push ((&mut self) (val i32))
        (rust { self.data.push(val); }))

    (fn pop ((&mut self)) OptInt
        (rust { self.data.pop() }))

    (fn is_empty ((&self)) bool
        (rust { self.data.is_empty() }))

    (fn size ((&self)) usize
        (rust { self.data.len() }))
);

// =============================================================================
// Tests
// =============================================================================

#[test]
fn test_tokenizer() {
    let tokens = lisp!(tokenize "3 + 4.5 * 2");
    assert_eq!(tokens.len(), 5);
    assert_eq!(tokens[0], Token::Num(3.0));
    assert_eq!(tokens[1], Token::Plus);
    assert_eq!(tokens[2], Token::Num(4.5));
    assert_eq!(tokens[3], Token::Star);
    assert_eq!(tokens[4], Token::Num(2.0));
}

#[test]
fn test_evaluator_literal() {
    let expr = lisp!(make_lit 42.0);
    let mut ev = Evaluator::new();
    let result = ev.eval(&expr);
    assert!((result - 42.0).abs() < 1e-10);
    assert_eq!(ev.get_call_count(), 1);
}

#[test]
fn test_evaluator_binop() {
    // (3 + 4) * 2 = 14
    let three = make_lit(3.0);
    let four = make_lit(4.0);
    let two = make_lit(2.0);
    let sum = make_binop(BinOpKind::Add, three, four);
    let product = make_binop(BinOpKind::Mul, sum, two);

    let mut ev = Evaluator::new();
    let result = ev.eval(&product);
    assert!((result - 14.0).abs() < 1e-10);
    // Should have visited: product, sum, 3, 4, 2 = 5 calls
    assert_eq!(ev.get_call_count(), 5);
}

#[test]
fn test_evaluator_neg() {
    let expr = make_neg(make_lit(7.0));
    let mut ev = Evaluator::new();
    let result = ev.eval(&expr);
    assert!((result - (-7.0_f64)).abs() < 1e-10);
}

#[test]
fn test_display_binop_kind() {
    assert_eq!(format!("{}", BinOpKind::Add), "+");
    assert_eq!(format!("{}", BinOpKind::Sub), "-");
    assert_eq!(format!("{}", BinOpKind::Mul), "*");
    assert_eq!(format!("{}", BinOpKind::Div), "/");
}

#[test]
fn test_display_expr() {
    // Display: (3 + 4)
    let expr = make_binop(BinOpKind::Add, make_lit(3.0), make_lit(4.0));
    let s = lisp!(format! "{}" expr);
    assert_eq!(s, "(3 + 4)");
}

#[test]
fn test_token_to_op() {
    assert_eq!(token_to_op(&Token::Plus), Some(BinOpKind::Add));
    assert_eq!(token_to_op(&Token::Minus), Some(BinOpKind::Sub));
    assert_eq!(token_to_op(&Token::Star), Some(BinOpKind::Mul));
    assert_eq!(token_to_op(&Token::Slash), Some(BinOpKind::Div));
    assert_eq!(token_to_op(&Token::Num(0.0)), None);
}

#[test]
fn test_sum_range() {
    // sum 1..11 = 55
    assert_eq!(lisp!(sum_range 1 11), 55);
    assert_eq!(lisp!(sum_range 0 1), 0);
    assert_eq!(lisp!(sum_range 0 0), 0);
}

#[test]
fn test_sum_while() {
    assert_eq!(lisp!(sum_while 10), 55);
    assert_eq!(lisp!(sum_while 0), 0);
    assert_eq!(lisp!(sum_while 1), 1);
}

#[test]
fn test_classify() {
    let neg = -5;
    assert_eq!(lisp!(classify neg), "negative");
    assert_eq!(lisp!(classify 0), "zero");
    assert_eq!(lisp!(classify 5), "small");
    assert_eq!(lisp!(classify 50), "medium");
    assert_eq!(lisp!(classify 999), "large");
}

#[test]
fn test_collatz_steps() {
    assert_eq!(lisp!(collatz_steps 1), 0);
    assert_eq!(lisp!(collatz_steps 2), 1);
    assert_eq!(lisp!(collatz_steps 27), 111);
}

#[test]
fn test_factorial_iter() {
    assert_eq!(lisp!(factorial_iter 0), 1);
    assert_eq!(lisp!(factorial_iter 1), 1);
    assert_eq!(lisp!(factorial_iter 5), 120);
    assert_eq!(lisp!(factorial_iter 10), 3628800);
}

#[test]
fn test_fibonacci() {
    let fibs: Vec<i32> = lisp!(block
        (let mut results (vec))
        (for i in (.. 0 10)
            (results.push (fib i)))
        (val results)
    );
    assert_eq!(fibs, vec![0, 1, 1, 2, 3, 5, 8, 13, 21, 34]);
}

#[test]
fn test_stats() {
    let mut stats = Stats::new();
    // Add values using a for loop over a vec
    let values = lisp!(vec 10.0 20.0 30.0 40.0 50.0);
    for v in &values {
        stats.add(*v);
    }
    assert_eq!(stats.count, 5);
    assert!((stats.mean() - 30.0).abs() < 1e-10);
    assert!((stats.range() - 40.0).abs() < 1e-10);
    assert!((stats.min - 10.0).abs() < 1e-10);
    assert!((stats.max - 50.0).abs() < 1e-10);

    let display = lisp!(format! "{}" stats);
    assert_eq!(display, "Stats(n=5, mean=30.00)");
}

#[test]
fn test_stack() {
    let mut stack = Stack::new();
    assert!(stack.is_empty());

    // Push values using lisp
    lisp!(block
        (stack.push 10)
        (stack.push 20)
        (stack.push 30)
    );

    assert_eq!(stack.size(), 3);
    assert_eq!(stack.pop(), Some(30));
    assert_eq!(stack.pop(), Some(20));
    assert_eq!(stack.pop(), Some(10));
    assert_eq!(stack.pop(), None);
    assert!(stack.is_empty());
}

#[test]
fn test_arithmetic_operators() {
    // All binary operators
    assert_eq!(lisp!(+ 10 20), 30);
    assert_eq!(lisp!(- 50 15), 35);
    assert_eq!(lisp!(* 6 7), 42);
    assert_eq!(lisp!(/ 100 4), 25);
    assert_eq!(lisp!(% 17 5), 2);

    // Variadic
    assert_eq!(lisp!(+ 1 2 3 4 5), 15);
    assert_eq!(lisp!(* 2 3 4), 24);

    // Nested
    let x = lisp!(+ (* 3 4) (- 10 5));
    assert_eq!(x, 17);
}

#[test]
fn test_comparison_and_logical() {
    assert!(lisp!(== 5 5));
    assert!(lisp!(!= 5 6));
    assert!(lisp!(< 1 2));
    assert!(lisp!(> 2 1));
    assert!(lisp!(<= 3 3));
    assert!(lisp!(>= 3 3));

    // Logical operators
    assert!(lisp!(&& true true));
    assert!(lisp!(|| false true));
    assert!(lisp!(! false));
    assert!(!lisp!(&& true false));
}

#[test]
fn test_compound_assignment() {
    lisp!(let mut x 100);
    lisp!(+= x 10);
    assert_eq!(x, 110);
    lisp!(-= x 20);
    assert_eq!(x, 90);
    lisp!(*= x 2);
    assert_eq!(x, 180);
    lisp!(/= x 3);
    assert_eq!(x, 60);
    lisp!(%= x 7);
    assert_eq!(x, 4);
}

#[test]
fn test_control_flow_comprehensive() {
    // Match expression
    let label: &str = lisp!(match 42
        (0 => "zero")
        (1 => "one")
        (_ => "other"));
    assert_eq!(label, "other");

    // While with break
    lisp!(let mut n 0);
    lisp!(loop
        (+= n 1)
        (if (>= n 10) (break)));
    assert_eq!(n, 10);

    // For with continue — sum only odd numbers 1..10
    let odd_sum = lisp!(block
        (let mut total 0)
        (for i in (.. 1 10)
            (if (== (% i 2) 0) (continue))
            (+= total i))
        (val total)
    );
    // 1+3+5+7+9 = 25
    assert_eq!(odd_sum, 25);
}

#[test]
fn test_vec_and_tuple() {
    let v = lisp!(vec 10 20 30);
    assert_eq!(v, vec![10, 20, 30]);

    let t = lisp!(tuple 1 2 3);
    assert_eq!(t, (1, 2, 3));
}

#[test]
fn test_format_macro() {
    let s = lisp!(format! "Hello, {}! {} + {} = {}" "world" 2 3 5);
    assert_eq!(s, "Hello, world! 2 + 3 = 5");
}

#[test]
fn test_closures_with_iterator() {
    let double = lisp!(fn ((x i32)) (* x 2));
    let results: Vec<i32> = (1..=5).map(double).collect();
    assert_eq!(results, vec![2, 4, 6, 8, 10]);

    let square = lisp!(fn ((x i32)) (* x x));
    let squares: Vec<i32> = (1..=5).map(square).collect();
    assert_eq!(squares, vec![1, 4, 9, 16, 25]);
}

#[test]
fn test_scoped_let_bindings() {
    let result = lisp!(let ((a 10) (b 20) (c 30))
        (+ (+ a b) c)
    );
    assert_eq!(result, 60);
}

#[test]
fn test_block_returns_value() {
    let x = lisp!(block
        (let a 10)
        (let b 20)
        (+ a b)
    );
    assert_eq!(x, 30);
}

#[test]
fn test_range_and_inclusive_range() {
    let r: Vec<i32> = lisp!(block
        (let mut v (vec))
        (for i in (.. 0 5) (v.push i))
        (val v)
    );
    assert_eq!(r, vec![0, 1, 2, 3, 4]);

    lisp!(let mut sum 0);
    lisp!(for i in (..= 1 10) (+= sum i));
    assert_eq!(sum, 55);
}

#[test]
fn test_rust_escape_hatch() {
    // Use rust escape for complex expressions the macro can't handle
    lisp!(rust let xs: Vec<i32> = (0..10).filter(|x| x % 2 == 0).collect());
    assert_eq!(xs, vec![0, 2, 4, 6, 8]);
}

#[test]
fn test_full_pipeline() {
    // End-to-end: tokenize, build AST manually, evaluate, display
    let tokens = lisp!(tokenize "3 + 4");
    assert_eq!(tokens.len(), 3);

    // Build AST: 3 + 4
    let expr = make_binop(BinOpKind::Add, make_lit(3.0), make_lit(4.0));

    // Evaluate
    let mut ev = Evaluator::new();
    let result = ev.eval(&expr);
    assert!((result - 7.0).abs() < 1e-10);

    // Display
    let display = lisp!(format! "{}" expr);
    assert_eq!(display, "(3 + 4)");

    // More complex: (3 + 4) * (10 - 5) = 35
    let lhs = make_binop(BinOpKind::Add, make_lit(3.0), make_lit(4.0));
    let rhs = make_binop(BinOpKind::Sub, make_lit(10.0), make_lit(5.0));
    let complex = make_binop(BinOpKind::Mul, lhs, rhs);

    let result = ev.eval(&complex);
    assert!((result - 35.0).abs() < 1e-10);

    let display = lisp!(format! "{}" complex);
    assert_eq!(display, "((3 + 4) * (10 - 5))");
}
