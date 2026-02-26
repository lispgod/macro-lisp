;; ============================================================
;; lisp_interpreter.lisp — A Lisp Interpreter in macro-lisp
;; ============================================================
;;
;; A meta-demonstration: using macro-lisp (a Lisp-like syntax
;; for Rust) to implement a Lisp interpreter. This file is
;; compiled by the macro-lisp CLI into Rust source code.
;;
;; Run with:
;;   cargo run -p macro-lisp-cli -- run scripts/lisp_interpreter.lisp
;;
;; The interpreter supports:
;;   - Numeric (integer & float), boolean, string, symbol, nil types
;;   - Lambda functions with lexical closures
;;   - Special forms: define, set!, lambda, fn, if, cond, let,
;;     begin, do, quote, and, or
;;   - Built-in arithmetic, comparison, list, string, and I/O ops
;;   - A REPL (read-eval-print loop) with a prompt
;;
;; ============================================================

;; ============================================================
;; Section 1: Use Statements
;; ============================================================

(use std::collections::HashMap)
(use std::fmt)
(use std::io)
(use std::io::Write)
(use std::io::BufRead)
(use std::rc::Rc)
(use std::cell::RefCell)

;; ============================================================
;; Section 2: Type Aliases
;; ============================================================

(type EnvRef = Rc<RefCell<Env>>)
(type LispError = String)
(type LispResult = Result<LispVal, LispError>)
(type ParseResult = (LispVal, usize))

;; ============================================================
;; Section 3: LambdaData — Closure Data for Lambda Values
;; ============================================================
;;
;; Holds the parameter names, body expression, and captured
;; environment for a lambda closure. Stored as a separate
;; struct because macro-lisp enum variants support only
;; single-type tuple variants or named-field struct variants.

(#[derive(Clone, Debug)]
struct LambdaData
    ((params Vec<String>)
     (body Box<LispVal>)
     (closure EnvRef)))

;; ============================================================
;; Section 4: LispVal — The Core Value Type
;; ============================================================
;;
;; Every value in our Lisp is represented as a LispVal.
;; Variants: Nil, Bool, Int, Float, Sym, Str, List,
;; Lambda (via LambdaData wrapper), and Builtin.

(#[derive(Clone, Debug)]
enum LispVal
    (Nil)
    (Bool bool)
    (Int i64)
    (Float f64)
    (Sym String)
    (Str String)
    (List Vec<LispVal>)
    (Lambda LambdaData)
    (Builtin String))

;; ============================================================
;; Section 5: Display for LispVal
;; ============================================================
;;
;; Pretty-printing Lisp values for REPL output.

(impl fmt::Display for LispVal
    (fn fmt ((&self) (f &mut fmt::Formatter<'_>)) fmt::Result
        (rust {
            match self {
                LispVal::Nil => write!(f, "nil"),
                LispVal::Bool(b) => {
                    if *b { write!(f, "#t") } else { write!(f, "#f") }
                }
                LispVal::Int(n) => write!(f, "{}", n),
                LispVal::Float(n) => write!(f, "{}", n),
                LispVal::Sym(s) => write!(f, "{}", s),
                LispVal::Str(s) => write!(f, "\"{}\"", s),
                LispVal::List(elems) => {
                    write!(f, "(")?;
                    for (i, e) in elems.iter().enumerate() {
                        if i > 0 { write!(f, " ")?; }
                        write!(f, "{}", e)?;
                    }
                    write!(f, ")")
                }
                LispVal::Lambda(data) => {
                    write!(f, "<lambda (")?;
                    for (i, p) in data.params.iter().enumerate() {
                        if i > 0 { write!(f, " ")?; }
                        write!(f, "{}", p)?;
                    }
                    write!(f, ")>")
                }
                LispVal::Builtin(name) => write!(f, "<builtin:{}>", name),
            }
        })))

;; ============================================================
;; Section 6: PartialEq for LispVal
;; ============================================================
;;
;; Structural equality for Lisp values used by equal? and
;; comparison builtins.

(impl PartialEq for LispVal
    (fn eq ((&self) (other &LispVal)) bool
        (rust {
            match (self, other) {
                (LispVal::Nil, LispVal::Nil) => true,
                (LispVal::Bool(a), LispVal::Bool(b)) => a == b,
                (LispVal::Int(a), LispVal::Int(b)) => a == b,
                (LispVal::Float(a), LispVal::Float(b)) => a == b,
                (LispVal::Int(a), LispVal::Float(b)) => (*a as f64) == *b,
                (LispVal::Float(a), LispVal::Int(b)) => *a == (*b as f64),
                (LispVal::Sym(a), LispVal::Sym(b)) => a == b,
                (LispVal::Str(a), LispVal::Str(b)) => a == b,
                (LispVal::List(a), LispVal::List(b)) => a == b,
                _ => false,
            }
        })))

;; ============================================================
;; Section 7: Environment — Lexical Scoping
;; ============================================================
;;
;; The environment holds variable bindings and a reference to
;; a parent environment, forming a scope chain for lexical
;; scoping. Each environment is wrapped in Rc<RefCell<>> for
;; shared mutable access.

(#[derive(Debug, Clone)]
struct Env
    ((bindings HashMap<String, LispVal>)
     (parent Option<EnvRef>)))

;; Create a new top-level root environment
(impl Env
    (fn new () EnvRef
        (rust {
            Rc::new(RefCell::new(Env {
                bindings: HashMap::new(),
                parent: None,
            }))
        }))
    (fn extend ((parent_env EnvRef)) EnvRef
        (rust {
            Rc::new(RefCell::new(Env {
                bindings: HashMap::new(),
                parent: Some(parent_env),
            }))
        }))
    (fn get ((&self) (name &str)) Option<LispVal>
        (rust {
            if let Some(val) = self.bindings.get(name) {
                Some(val.clone())
            } else if let Some(ref parent) = self.parent {
                parent.borrow().get(name)
            } else {
                None
            }
        }))
    (fn define ((&mut self) (name String) (val LispVal)) ()
        (rust {
            self.bindings.insert(name, val);
        }))
    (fn set ((&mut self) (name &str) (val LispVal)) bool
        (rust {
            if self.bindings.contains_key(name) {
                self.bindings.insert(name.to_string(), val);
                true
            } else if let Some(ref parent) = self.parent {
                parent.borrow_mut().set(name, val)
            } else {
                false
            }
        })))

;; ============================================================
;; Section 8: Token — Lexer Token Type
;; ============================================================
;;
;; Tokens produced by the tokenizer: parentheses, quote sugar,
;; strings, and atoms.

(#[derive(Debug, Clone, PartialEq)]
enum Token
    (LParen)
    (RParen)
    (Quote)
    (Atom String)
    (StringTok String))

;; ============================================================
;; Section 9: Tokenizer
;; ============================================================
;;
;; Converts raw Lisp source text into a vector of tokens.
;; Handles parentheses, string literals with escapes,
;; line comments starting with ;, quote sugar, and atoms.

(fn tokenize ((input &str)) Vec<Token>
    (rust {
        let mut tokens: Vec<Token> = Vec::new();
        let chars: Vec<char> = input.chars().collect();
        let len = chars.len();
        let mut i: usize = 0;

        while i < len {
            let ch = chars[i];
            match ch {
                // Skip whitespace
                ' ' | '\t' | '\n' | '\r' => {
                    i += 1;
                }
                // Line comment: skip to end of line
                ';' => {
                    while i < len && chars[i] != '\n' {
                        i += 1;
                    }
                }
                // Left parenthesis
                '(' => {
                    tokens.push(Token::LParen);
                    i += 1;
                }
                // Right parenthesis
                ')' => {
                    tokens.push(Token::RParen);
                    i += 1;
                }
                // Quote sugar: 'expr => (quote expr)
                '\'' => {
                    tokens.push(Token::Quote);
                    i += 1;
                }
                // String literal
                '"' => {
                    i += 1; // skip opening quote
                    let mut s = String::new();
                    while i < len && chars[i] != '"' {
                        if chars[i] == '\\' && i + 1 < len {
                            i += 1;
                            match chars[i] {
                                'n' => s.push('\n'),
                                't' => s.push('\t'),
                                '\\' => s.push('\\'),
                                '"' => s.push('"'),
                                other => {
                                    s.push('\\');
                                    s.push(other);
                                }
                            }
                        } else {
                            s.push(chars[i]);
                        }
                        i += 1;
                    }
                    if i < len {
                        i += 1; // skip closing quote
                    }
                    tokens.push(Token::StringTok(s));
                }
                // Atom: symbol, number, boolean, etc.
                _ => {
                    let start = i;
                    while i < len
                        && chars[i] != ' '
                        && chars[i] != '\t'
                        && chars[i] != '\n'
                        && chars[i] != '\r'
                        && chars[i] != '('
                        && chars[i] != ')'
                        && chars[i] != '"'
                        && chars[i] != ';'
                    {
                        i += 1;
                    }
                    let atom: String = chars[start..i].iter().collect();
                    if !atom.is_empty() {
                        tokens.push(Token::Atom(atom));
                    }
                }
            }
        }
        tokens
    }))

;; ============================================================
;; Section 10: Parser
;; ============================================================
;;
;; Converts a flat token stream into a nested LispVal AST.
;; Uses a recursive descent approach with an index cursor.

;; Parse a single expression from the token stream at position pos.
;; Returns the parsed LispVal and the new position.
(fn parse_expr ((tokens &Vec<Token>) (pos usize)) ParseResult
    (rust {
        if pos >= tokens.len() {
            return (LispVal::Nil, pos);
        }
        match &tokens[pos] {
            // Quote sugar: 'x => (quote x)
            Token::Quote => {
                let (inner, next_pos) = parse_expr(tokens, pos + 1);
                let quoted = LispVal::List(vec![
                    LispVal::Sym("quote".to_string()),
                    inner,
                ]);
                (quoted, next_pos)
            }
            // List: ( expr* )
            Token::LParen => {
                let mut elems: Vec<LispVal> = Vec::new();
                let mut cur = pos + 1;
                while cur < tokens.len() {
                    if tokens[cur] == Token::RParen {
                        cur += 1;
                        return (LispVal::List(elems), cur);
                    }
                    let (val, next) = parse_expr(tokens, cur);
                    elems.push(val);
                    cur = next;
                }
                // Unmatched paren: return what we have
                (LispVal::List(elems), cur)
            }
            // Unexpected closing paren
            Token::RParen => {
                (LispVal::Nil, pos + 1)
            }
            // String literal
            Token::StringTok(s) => {
                (LispVal::Str(s.clone()), pos + 1)
            }
            // Atom: integer, float, bool, nil, or symbol
            Token::Atom(s) => {
                let val = parse_atom(s);
                (val, pos + 1)
            }
        }
    }))

;; Parse an atom string into the appropriate LispVal type
(fn parse_atom ((s &str)) LispVal
    (rust {
        if let Ok(n) = s.parse::<i64>() {
            return LispVal::Int(n);
        }
        if let Ok(f) = s.parse::<f64>() {
            return LispVal::Float(f);
        }
        match s {
            "#t" | "true" => LispVal::Bool(true),
            "#f" | "false" => LispVal::Bool(false),
            "nil" => LispVal::Nil,
            _ => LispVal::Sym(s.to_string()),
        }
    }))

;; Parse all expressions from a source string
(fn parse ((input &str)) Vec<LispVal>
    (rust {
        let tokens = tokenize(input);
        let mut results: Vec<LispVal> = Vec::new();
        let mut pos: usize = 0;
        while pos < tokens.len() {
            let (val, next_pos) = parse_expr(&tokens, pos);
            results.push(val);
            pos = next_pos;
        }
        results
    }))

;; ============================================================
;; Section 11: Error Helpers
;; ============================================================

;; Helper to create an error result
(fn lisp_err ((msg &str)) LispResult
    (rust { Err(msg.to_string()) }))

;; ============================================================
;; Section 12: Evaluator — Core eval Function
;; ============================================================
;;
;; The heart of the interpreter. Evaluates a LispVal in the
;; context of an environment. Handles special forms and
;; delegates function application to apply_func.

(fn eval ((expr &LispVal) (env &EnvRef)) LispResult
    (rust {
        match expr {
            // Self-evaluating forms
            LispVal::Nil => Ok(LispVal::Nil),
            LispVal::Bool(_) => Ok(expr.clone()),
            LispVal::Int(_) => Ok(expr.clone()),
            LispVal::Float(_) => Ok(expr.clone()),
            LispVal::Str(_) => Ok(expr.clone()),
            LispVal::Builtin(_) => Ok(expr.clone()),
            LispVal::Lambda(_) => Ok(expr.clone()),

            // Symbol: look up in environment
            LispVal::Sym(name) => {
                match env.borrow().get(name) {
                    Some(val) => Ok(val),
                    None => Err(format!("Undefined symbol: {}", name)),
                }
            }

            // List: either a special form or function call
            LispVal::List(elems) => {
                if elems.is_empty() {
                    return Ok(LispVal::Nil);
                }

                // Check for special forms by examining the head
                if let LispVal::Sym(ref head) = elems[0] {
                    match head.as_str() {
                        "quote"  => eval_quote(elems),
                        "if"     => eval_if(elems, env),
                        "cond"   => eval_cond(elems, env),
                        "define" => eval_define(elems, env),
                        "set!"   => eval_set(elems, env),
                        "lambda" | "fn" => eval_lambda(elems, env),
                        "let"    => eval_let(elems, env),
                        "begin" | "do" => eval_begin(elems, env),
                        "and"    => eval_and(elems, env),
                        "or"     => eval_or(elems, env),
                        _ => {
                            // Regular function application
                            eval_application(elems, env)
                        }
                    }
                } else {
                    // Head is not a symbol: evaluate and apply
                    eval_application(elems, env)
                }
            }
        }
    }))

;; ============================================================
;; Section 13: Special Forms
;; ============================================================

;; (quote expr) => return expr unevaluated
(fn eval_quote ((elems &Vec<LispVal>)) LispResult
    (rust {
        if elems.len() != 2 {
            return Err("quote requires exactly one argument".to_string());
        }
        Ok(elems[1].clone())
    }))

;; (if condition then-expr else-expr)
;; (if condition then-expr)
(fn eval_if ((elems &Vec<LispVal>) (env &EnvRef)) LispResult
    (rust {
        if elems.len() < 3 || elems.len() > 4 {
            return Err("if requires 2 or 3 arguments".to_string());
        }
        let cond_val = eval(&elems[1], env)?;
        let is_truthy = match cond_val {
            LispVal::Bool(false) => false,
            LispVal::Nil => false,
            _ => true,
        };
        if is_truthy {
            eval(&elems[2], env)
        } else if elems.len() == 4 {
            eval(&elems[3], env)
        } else {
            Ok(LispVal::Nil)
        }
    }))

;; (cond (test1 expr1) (test2 expr2) ... (else exprN))
(fn eval_cond ((elems &Vec<LispVal>) (env &EnvRef)) LispResult
    (rust {
        for clause in elems.iter().skip(1) {
            if let LispVal::List(pair) = clause {
                if pair.len() < 2 {
                    return Err("cond clause must have test and expression".to_string());
                }
                // Check for else clause
                let is_else = matches!(&pair[0], LispVal::Sym(s) if s == "else");
                if is_else {
                    let mut result = LispVal::Nil;
                    for expr in pair.iter().skip(1) {
                        result = eval(expr, env)?;
                    }
                    return Ok(result);
                }
                let test_val = eval(&pair[0], env)?;
                let is_truthy = match test_val {
                    LispVal::Bool(false) | LispVal::Nil => false,
                    _ => true,
                };
                if is_truthy {
                    let mut result = LispVal::Nil;
                    for expr in pair.iter().skip(1) {
                        result = eval(expr, env)?;
                    }
                    return Ok(result);
                }
            } else {
                return Err("cond clause must be a list".to_string());
            }
        }
        Ok(LispVal::Nil)
    }))

;; (define name value)
;; (define (name params...) body...) => sugar for lambda
(fn eval_define ((elems &Vec<LispVal>) (env &EnvRef)) LispResult
    (rust {
        if elems.len() < 3 {
            return Err("define requires at least 2 arguments".to_string());
        }
        match &elems[1] {
            // Simple variable definition: (define x 42)
            LispVal::Sym(name) => {
                let val = eval(&elems[2], env)?;
                env.borrow_mut().define(name.clone(), val.clone());
                Ok(val)
            }
            // Function shorthand: (define (f x y) body...)
            LispVal::List(sig) => {
                if sig.is_empty() {
                    return Err("define: function signature cannot be empty".to_string());
                }
                let name = match &sig[0] {
                    LispVal::Sym(s) => s.clone(),
                    _ => return Err("define: function name must be a symbol".to_string()),
                };
                let params: Vec<String> = sig[1..].iter().map(|p| {
                    match p {
                        LispVal::Sym(s) => s.clone(),
                        _ => "?".to_string(),
                    }
                }).collect();
                // Wrap body in begin if multiple expressions
                let body = if elems.len() == 3 {
                    elems[2].clone()
                } else {
                    let mut begin_elems = vec![LispVal::Sym("begin".to_string())];
                    for expr in elems[2..].iter() {
                        begin_elems.push(expr.clone());
                    }
                    LispVal::List(begin_elems)
                };
                let lambda = LispVal::Lambda(LambdaData {
                    params,
                    body: Box::new(body),
                    closure: env.clone(),
                });
                env.borrow_mut().define(name, lambda.clone());
                Ok(lambda)
            }
            _ => Err("define: first argument must be a symbol or list".to_string()),
        }
    }))

;; (set! name value) => update existing binding
(fn eval_set ((elems &Vec<LispVal>) (env &EnvRef)) LispResult
    (rust {
        if elems.len() != 3 {
            return Err("set! requires exactly 2 arguments".to_string());
        }
        let name = match &elems[1] {
            LispVal::Sym(s) => s.clone(),
            _ => return Err("set!: first argument must be a symbol".to_string()),
        };
        let val = eval(&elems[2], env)?;
        if env.borrow_mut().set(&name, val.clone()) {
            Ok(val)
        } else {
            Err(format!("set!: undefined variable: {}", name))
        }
    }))

;; (lambda (params...) body...)
;; (fn (params...) body...)
(fn eval_lambda ((elems &Vec<LispVal>) (env &EnvRef)) LispResult
    (rust {
        if elems.len() < 3 {
            return Err("lambda requires parameter list and body".to_string());
        }
        let params = match &elems[1] {
            LispVal::List(p) => {
                p.iter().map(|x| match x {
                    LispVal::Sym(s) => s.clone(),
                    _ => "?".to_string(),
                }).collect::<Vec<String>>()
            }
            _ => return Err("lambda: first argument must be a parameter list".to_string()),
        };
        // Wrap body in begin if multiple expressions
        let body = if elems.len() == 3 {
            elems[2].clone()
        } else {
            let mut begin_elems = vec![LispVal::Sym("begin".to_string())];
            for expr in elems[2..].iter() {
                begin_elems.push(expr.clone());
            }
            LispVal::List(begin_elems)
        };
        Ok(LispVal::Lambda(LambdaData {
            params,
            body: Box::new(body),
            closure: env.clone(),
        }))
    }))

;; (let ((x 1) (y 2)) body...)
;; Creates a new scope with the given bindings
(fn eval_let ((elems &Vec<LispVal>) (env &EnvRef)) LispResult
    (rust {
        if elems.len() < 3 {
            return Err("let requires bindings and body".to_string());
        }
        let bindings = match &elems[1] {
            LispVal::List(b) => b.clone(),
            _ => return Err("let: first argument must be a binding list".to_string()),
        };
        // Create a new child environment for the let scope
        let let_env = Env::extend(env.clone());
        for binding in bindings.iter() {
            match binding {
                LispVal::List(pair) => {
                    if pair.len() != 2 {
                        return Err("let: each binding must be (name value)".to_string());
                    }
                    let name = match &pair[0] {
                        LispVal::Sym(s) => s.clone(),
                        _ => return Err("let: binding name must be a symbol".to_string()),
                    };
                    let val = eval(&pair[1], env)?;
                    let_env.borrow_mut().define(name, val);
                }
                _ => return Err("let: each binding must be a list".to_string()),
            }
        }
        // Evaluate body in the let environment
        let mut result = LispVal::Nil;
        for expr in elems[2..].iter() {
            result = eval(expr, &let_env)?;
        }
        Ok(result)
    }))

;; (begin expr1 expr2 ...) => evaluate in order, return last
(fn eval_begin ((elems &Vec<LispVal>) (env &EnvRef)) LispResult
    (rust {
        let mut result = LispVal::Nil;
        for expr in elems.iter().skip(1) {
            result = eval(expr, env)?;
        }
        Ok(result)
    }))

;; (and expr1 expr2 ...) => short-circuit logical and
(fn eval_and ((elems &Vec<LispVal>) (env &EnvRef)) LispResult
    (rust {
        let mut result = LispVal::Bool(true);
        for expr in elems.iter().skip(1) {
            result = eval(expr, env)?;
            let is_falsy = matches!(&result, LispVal::Bool(false) | LispVal::Nil);
            if is_falsy {
                return Ok(result);
            }
        }
        Ok(result)
    }))

;; (or expr1 expr2 ...) => short-circuit logical or
(fn eval_or ((elems &Vec<LispVal>) (env &EnvRef)) LispResult
    (rust {
        let mut result = LispVal::Bool(false);
        for expr in elems.iter().skip(1) {
            result = eval(expr, env)?;
            let is_truthy = !matches!(&result, LispVal::Bool(false) | LispVal::Nil);
            if is_truthy {
                return Ok(result);
            }
        }
        Ok(result)
    }))

;; ============================================================
;; Section 14: Function Application
;; ============================================================
;;
;; Evaluate function calls: evaluate the head and arguments,
;; then dispatch to either a builtin or a lambda.

(fn eval_application ((elems &Vec<LispVal>) (env &EnvRef)) LispResult
    (rust {
        let func = eval(&elems[0], env)?;
        let mut args: Vec<LispVal> = Vec::new();
        for arg_expr in elems[1..].iter() {
            let val = eval(arg_expr, env)?;
            args.push(val);
        }
        apply_func(&func, &args)
    }))

;; Apply a function value to evaluated arguments
(fn apply_func ((func &LispVal) (args &Vec<LispVal>)) LispResult
    (rust {
        match func {
            LispVal::Builtin(name) => {
                call_builtin(name, args)
            }
            LispVal::Lambda(data) => {
                if data.params.len() != args.len() {
                    return Err(format!(
                        "Lambda expects {} arguments, got {}",
                        data.params.len(), args.len()
                    ));
                }
                let call_env = Env::extend(data.closure.clone());
                for (param, arg) in data.params.iter().zip(args.iter()) {
                    call_env.borrow_mut().define(param.clone(), arg.clone());
                }
                eval(&data.body, &call_env)
            }
            _ => Err(format!("Not a function: {}", func)),
        }
    }))

;; ============================================================
;; Section 15: Builtin Dispatch
;; ============================================================
;;
;; Routes builtin function names to their implementations.

(fn call_builtin ((name &str) (args &Vec<LispVal>)) LispResult
    (rust {
        match name {
            // Arithmetic
            "+"       => builtin_add(args),
            "-"       => builtin_sub(args),
            "*"       => builtin_mul(args),
            "/"       => builtin_div(args),
            "modulo"  => builtin_modulo(args),
            "abs"     => builtin_abs(args),
            "min"     => builtin_min(args),
            "max"     => builtin_max(args),

            // Comparison
            "="       => builtin_eq(args),
            "<"       => builtin_lt(args),
            ">"       => builtin_gt(args),
            "<="      => builtin_le(args),
            ">="      => builtin_ge(args),

            // Logic
            "not"     => builtin_not(args),

            // List operations
            "cons"    => builtin_cons(args),
            "car"     => builtin_car(args),
            "cdr"     => builtin_cdr(args),
            "list"    => builtin_list(args),
            "length"  => builtin_length(args),
            "append"  => builtin_append(args),
            "map"     => builtin_map(args),
            "filter"  => builtin_filter(args),
            "reduce"  => builtin_reduce(args),
            "reverse" => builtin_reverse(args),
            "nth"     => builtin_nth(args),
            "range"   => builtin_range(args),

            // Predicates
            "null?"    => builtin_is_null(args),
            "list?"    => builtin_is_list(args),
            "number?"  => builtin_is_number(args),
            "string?"  => builtin_is_string(args),
            "boolean?" => builtin_is_boolean(args),
            "symbol?"  => builtin_is_symbol(args),
            "pair?"    => builtin_is_pair(args),
            "equal?"   => builtin_is_equal(args),
            "zero?"    => builtin_is_zero(args),
            "positive?" => builtin_is_positive(args),
            "negative?" => builtin_is_negative(args),
            "procedure?" => builtin_is_procedure(args),

            // String operations
            "string-length"  => builtin_string_length(args),
            "string-append"  => builtin_string_append(args),
            "substring"      => builtin_substring(args),
            "string->number" => builtin_string_to_number(args),
            "number->string" => builtin_number_to_string(args),
            "string-ref"     => builtin_string_ref(args),
            "string-upcase"  => builtin_string_upcase(args),
            "string-downcase" => builtin_string_downcase(args),
            "string-contains" => builtin_string_contains(args),

            // I/O
            "display" => builtin_display(args),
            "newline" => builtin_newline(args),
            "print"   => builtin_print(args),
            "read"    => builtin_read(args),

            // Type conversions
            "exact->inexact" => builtin_to_float(args),
            "inexact->exact" => builtin_to_int(args),

            // Utility
            "apply"   => builtin_apply(args),
            "error"   => builtin_error(args),

            _ => Err(format!("Unknown builtin: {}", name)),
        }
    }))

;; ============================================================
;; Section 16: Built-in Arithmetic Functions
;; ============================================================

;; Helper: extract numeric value as f64
(fn to_f64 ((val &LispVal)) Result<f64, LispError>
    (rust {
        match val {
            LispVal::Int(n) => Ok(*n as f64),
            LispVal::Float(f) => Ok(*f),
            _ => Err(format!("Expected number, got: {}", val)),
        }
    }))

;; Helper: check if a value is an integer
(fn is_int ((val &LispVal)) bool
    (rust {
        matches!(val, LispVal::Int(_))
    }))

;; (+) => 0, (+ a) => a, (+ a b ...) => sum
(fn builtin_add ((args &Vec<LispVal>)) LispResult
    (rust {
        if args.is_empty() {
            return Ok(LispVal::Int(0));
        }
        let all_ints = args.iter().all(|a| is_int(a));
        if all_ints {
            let mut sum: i64 = 0;
            for arg in args {
                if let LispVal::Int(n) = arg {
                    sum += n;
                }
            }
            Ok(LispVal::Int(sum))
        } else {
            let mut sum: f64 = 0.0;
            for arg in args {
                sum += to_f64(arg)?;
            }
            Ok(LispVal::Float(sum))
        }
    }))

;; (-) => error, (- a) => -a, (- a b ...) => a - b - ...
(fn builtin_sub ((args &Vec<LispVal>)) LispResult
    (rust {
        if args.is_empty() {
            return Err("-: requires at least one argument".to_string());
        }
        if args.len() == 1 {
            return match &args[0] {
                LispVal::Int(n) => Ok(LispVal::Int(-n)),
                LispVal::Float(f) => Ok(LispVal::Float(-f)),
                _ => Err(format!("-: expected number, got: {}", args[0])),
            };
        }
        let all_ints = args.iter().all(|a| is_int(a));
        if all_ints {
            let mut result: i64 = if let LispVal::Int(n) = args[0] { n } else { 0 };
            for arg in args[1..].iter() {
                if let LispVal::Int(n) = arg {
                    result -= n;
                }
            }
            Ok(LispVal::Int(result))
        } else {
            let mut result = to_f64(&args[0])?;
            for arg in args[1..].iter() {
                result -= to_f64(arg)?;
            }
            Ok(LispVal::Float(result))
        }
    }))

;; (*) => 1, (* a) => a, (* a b ...) => product
(fn builtin_mul ((args &Vec<LispVal>)) LispResult
    (rust {
        if args.is_empty() {
            return Ok(LispVal::Int(1));
        }
        let all_ints = args.iter().all(|a| is_int(a));
        if all_ints {
            let mut product: i64 = 1;
            for arg in args {
                if let LispVal::Int(n) = arg {
                    product *= n;
                }
            }
            Ok(LispVal::Int(product))
        } else {
            let mut product: f64 = 1.0;
            for arg in args {
                product *= to_f64(arg)?;
            }
            Ok(LispVal::Float(product))
        }
    }))

;; (/ a b) => a / b
(fn builtin_div ((args &Vec<LispVal>)) LispResult
    (rust {
        if args.len() < 2 {
            return Err("/: requires at least 2 arguments".to_string());
        }
        let all_ints = args.iter().all(|a| is_int(a));
        if all_ints {
            let mut result: i64 = if let LispVal::Int(n) = args[0] { n } else { 0 };
            for arg in args[1..].iter() {
                if let LispVal::Int(n) = arg {
                    if *n == 0 {
                        return Err("/: division by zero".to_string());
                    }
                    result /= n;
                }
            }
            Ok(LispVal::Int(result))
        } else {
            let mut result = to_f64(&args[0])?;
            for arg in args[1..].iter() {
                let d = to_f64(arg)?;
                if d == 0.0 {
                    return Err("/: division by zero".to_string());
                }
                result /= d;
            }
            Ok(LispVal::Float(result))
        }
    }))

;; (modulo a b) => a % b
(fn builtin_modulo ((args &Vec<LispVal>)) LispResult
    (rust {
        if args.len() != 2 {
            return Err("modulo: requires exactly 2 arguments".to_string());
        }
        match (&args[0], &args[1]) {
            (LispVal::Int(a), LispVal::Int(b)) => {
                if *b == 0 {
                    Err("modulo: division by zero".to_string())
                } else {
                    Ok(LispVal::Int(a % b))
                }
            }
            _ => {
                let a = to_f64(&args[0])?;
                let b = to_f64(&args[1])?;
                if b == 0.0 {
                    Err("modulo: division by zero".to_string())
                } else {
                    Ok(LispVal::Float(a % b))
                }
            }
        }
    }))

;; (abs n) => absolute value
(fn builtin_abs ((args &Vec<LispVal>)) LispResult
    (rust {
        if args.len() != 1 {
            return Err("abs: requires exactly 1 argument".to_string());
        }
        match &args[0] {
            LispVal::Int(n) => Ok(LispVal::Int(n.abs())),
            LispVal::Float(f) => Ok(LispVal::Float(f.abs())),
            _ => Err(format!("abs: expected number, got: {}", args[0])),
        }
    }))

;; (min a b) => smaller value
(fn builtin_min ((args &Vec<LispVal>)) LispResult
    (rust {
        if args.len() != 2 {
            return Err("min: requires exactly 2 arguments".to_string());
        }
        let a = to_f64(&args[0])?;
        let b = to_f64(&args[1])?;
        if a <= b {
            Ok(args[0].clone())
        } else {
            Ok(args[1].clone())
        }
    }))

;; (max a b) => larger value
(fn builtin_max ((args &Vec<LispVal>)) LispResult
    (rust {
        if args.len() != 2 {
            return Err("max: requires exactly 2 arguments".to_string());
        }
        let a = to_f64(&args[0])?;
        let b = to_f64(&args[1])?;
        if a >= b {
            Ok(args[0].clone())
        } else {
            Ok(args[1].clone())
        }
    }))

;; ============================================================
;; Section 17: Built-in Comparison Functions
;; ============================================================

;; (= a b) => equality
(fn builtin_eq ((args &Vec<LispVal>)) LispResult
    (rust {
        if args.len() != 2 {
            return Err("=: requires exactly 2 arguments".to_string());
        }
        Ok(LispVal::Bool(args[0] == args[1]))
    }))

;; (< a b) => less than
(fn builtin_lt ((args &Vec<LispVal>)) LispResult
    (rust {
        if args.len() != 2 {
            return Err("<: requires exactly 2 arguments".to_string());
        }
        let a = to_f64(&args[0])?;
        let b = to_f64(&args[1])?;
        Ok(LispVal::Bool(a < b))
    }))

;; (> a b) => greater than
(fn builtin_gt ((args &Vec<LispVal>)) LispResult
    (rust {
        if args.len() != 2 {
            return Err(">: requires exactly 2 arguments".to_string());
        }
        let a = to_f64(&args[0])?;
        let b = to_f64(&args[1])?;
        Ok(LispVal::Bool(a > b))
    }))

;; (<= a b) => less than or equal
(fn builtin_le ((args &Vec<LispVal>)) LispResult
    (rust {
        if args.len() != 2 {
            return Err("<=: requires exactly 2 arguments".to_string());
        }
        let a = to_f64(&args[0])?;
        let b = to_f64(&args[1])?;
        Ok(LispVal::Bool(a <= b))
    }))

;; (>= a b) => greater than or equal
(fn builtin_ge ((args &Vec<LispVal>)) LispResult
    (rust {
        if args.len() != 2 {
            return Err(">=: requires exactly 2 arguments".to_string());
        }
        let a = to_f64(&args[0])?;
        let b = to_f64(&args[1])?;
        Ok(LispVal::Bool(a >= b))
    }))

;; ============================================================
;; Section 18: Built-in Logic Functions
;; ============================================================

;; (not x) => boolean negation
(fn builtin_not ((args &Vec<LispVal>)) LispResult
    (rust {
        if args.len() != 1 {
            return Err("not: requires exactly 1 argument".to_string());
        }
        let result = matches!(&args[0], LispVal::Bool(false) | LispVal::Nil);
        Ok(LispVal::Bool(result))
    }))

;; ============================================================
;; Section 19: Built-in List Functions
;; ============================================================

;; (cons head tail) => prepend to list
(fn builtin_cons ((args &Vec<LispVal>)) LispResult
    (rust {
        if args.len() != 2 {
            return Err("cons: requires exactly 2 arguments".to_string());
        }
        match &args[1] {
            LispVal::List(tail) => {
                let mut new_list = vec![args[0].clone()];
                new_list.extend(tail.iter().cloned());
                Ok(LispVal::List(new_list))
            }
            LispVal::Nil => {
                Ok(LispVal::List(vec![args[0].clone()]))
            }
            _ => {
                // Cons pair as a 2-element list
                Ok(LispVal::List(vec![args[0].clone(), args[1].clone()]))
            }
        }
    }))

;; (car lst) => first element
(fn builtin_car ((args &Vec<LispVal>)) LispResult
    (rust {
        if args.len() != 1 {
            return Err("car: requires exactly 1 argument".to_string());
        }
        match &args[0] {
            LispVal::List(elems) => {
                if elems.is_empty() {
                    Err("car: cannot take car of empty list".to_string())
                } else {
                    Ok(elems[0].clone())
                }
            }
            _ => Err(format!("car: expected list, got: {}", args[0])),
        }
    }))

;; (cdr lst) => rest of list
(fn builtin_cdr ((args &Vec<LispVal>)) LispResult
    (rust {
        if args.len() != 1 {
            return Err("cdr: requires exactly 1 argument".to_string());
        }
        match &args[0] {
            LispVal::List(elems) => {
                if elems.is_empty() {
                    Err("cdr: cannot take cdr of empty list".to_string())
                } else {
                    Ok(LispVal::List(elems[1..].to_vec()))
                }
            }
            _ => Err(format!("cdr: expected list, got: {}", args[0])),
        }
    }))

;; (list a b c ...) => create a new list
(fn builtin_list ((args &Vec<LispVal>)) LispResult
    (rust {
        Ok(LispVal::List(args.clone()))
    }))

;; (length lst) => number of elements
(fn builtin_length ((args &Vec<LispVal>)) LispResult
    (rust {
        if args.len() != 1 {
            return Err("length: requires exactly 1 argument".to_string());
        }
        match &args[0] {
            LispVal::List(elems) => Ok(LispVal::Int(elems.len() as i64)),
            LispVal::Str(s) => Ok(LispVal::Int(s.len() as i64)),
            LispVal::Nil => Ok(LispVal::Int(0)),
            _ => Err(format!("length: expected list or string, got: {}", args[0])),
        }
    }))

;; (append lst1 lst2 ...) => concatenate lists
(fn builtin_append ((args &Vec<LispVal>)) LispResult
    (rust {
        let mut result: Vec<LispVal> = Vec::new();
        for arg in args {
            match arg {
                LispVal::List(elems) => result.extend(elems.iter().cloned()),
                LispVal::Nil => {}
                other => result.push(other.clone()),
            }
        }
        Ok(LispVal::List(result))
    }))

;; (map func lst) => apply func to each element
(fn builtin_map ((args &Vec<LispVal>)) LispResult
    (rust {
        if args.len() != 2 {
            return Err("map: requires exactly 2 arguments (function, list)".to_string());
        }
        let func = &args[0];
        let list = match &args[1] {
            LispVal::List(elems) => elems.clone(),
            _ => return Err(format!("map: expected list, got: {}", args[1])),
        };
        let mut results: Vec<LispVal> = Vec::new();
        for elem in list.iter() {
            let result = apply_func(func, &vec![elem.clone()])?;
            results.push(result);
        }
        Ok(LispVal::List(results))
    }))

;; (filter func lst) => keep elements where func returns truthy
(fn builtin_filter ((args &Vec<LispVal>)) LispResult
    (rust {
        if args.len() != 2 {
            return Err("filter: requires exactly 2 arguments (function, list)".to_string());
        }
        let func = &args[0];
        let list = match &args[1] {
            LispVal::List(elems) => elems.clone(),
            _ => return Err(format!("filter: expected list, got: {}", args[1])),
        };
        let mut results: Vec<LispVal> = Vec::new();
        for elem in list.iter() {
            let test = apply_func(func, &vec![elem.clone()])?;
            let is_truthy = !matches!(&test, LispVal::Bool(false) | LispVal::Nil);
            if is_truthy {
                results.push(elem.clone());
            }
        }
        Ok(LispVal::List(results))
    }))

;; (reduce func init lst) => fold left
(fn builtin_reduce ((args &Vec<LispVal>)) LispResult
    (rust {
        if args.len() != 3 {
            return Err("reduce: requires exactly 3 arguments (function, initial, list)".to_string());
        }
        let func = &args[0];
        let mut acc = args[1].clone();
        let list = match &args[2] {
            LispVal::List(elems) => elems.clone(),
            _ => return Err(format!("reduce: expected list, got: {}", args[2])),
        };
        for elem in list.iter() {
            acc = apply_func(func, &vec![acc, elem.clone()])?;
        }
        Ok(acc)
    }))

;; (reverse lst) => reversed list
(fn builtin_reverse ((args &Vec<LispVal>)) LispResult
    (rust {
        if args.len() != 1 {
            return Err("reverse: requires exactly 1 argument".to_string());
        }
        match &args[0] {
            LispVal::List(elems) => {
                let mut reversed = elems.clone();
                reversed.reverse();
                Ok(LispVal::List(reversed))
            }
            _ => Err(format!("reverse: expected list, got: {}", args[0])),
        }
    }))

;; (nth n lst) => element at index n
(fn builtin_nth ((args &Vec<LispVal>)) LispResult
    (rust {
        if args.len() != 2 {
            return Err("nth: requires exactly 2 arguments (index, list)".to_string());
        }
        let idx = match &args[0] {
            LispVal::Int(n) => *n as usize,
            _ => return Err(format!("nth: expected integer index, got: {}", args[0])),
        };
        match &args[1] {
            LispVal::List(elems) => {
                if idx < elems.len() {
                    Ok(elems[idx].clone())
                } else {
                    Err(format!("nth: index {} out of bounds for list of length {}", idx, elems.len()))
                }
            }
            _ => Err(format!("nth: expected list, got: {}", args[1])),
        }
    }))

;; (range start end) => list of integers [start, end)
(fn builtin_range ((args &Vec<LispVal>)) LispResult
    (rust {
        if args.len() != 2 {
            return Err("range: requires exactly 2 arguments (start, end)".to_string());
        }
        let start = match &args[0] {
            LispVal::Int(n) => *n,
            _ => return Err(format!("range: expected integer, got: {}", args[0])),
        };
        let end = match &args[1] {
            LispVal::Int(n) => *n,
            _ => return Err(format!("range: expected integer, got: {}", args[1])),
        };
        let mut result: Vec<LispVal> = Vec::new();
        let mut i = start;
        while i < end {
            result.push(LispVal::Int(i));
            i += 1;
        }
        Ok(LispVal::List(result))
    }))

;; ============================================================
;; Section 20: Built-in Predicate Functions
;; ============================================================

;; (null? x) => is x nil or empty list?
(fn builtin_is_null ((args &Vec<LispVal>)) LispResult
    (rust {
        if args.len() != 1 {
            return Err("null?: requires exactly 1 argument".to_string());
        }
        let result = match &args[0] {
            LispVal::Nil => true,
            LispVal::List(elems) => elems.is_empty(),
            _ => false,
        };
        Ok(LispVal::Bool(result))
    }))

;; (list? x) => is x a list?
(fn builtin_is_list ((args &Vec<LispVal>)) LispResult
    (rust {
        if args.len() != 1 {
            return Err("list?: requires exactly 1 argument".to_string());
        }
        Ok(LispVal::Bool(matches!(&args[0], LispVal::List(_))))
    }))

;; (number? x) => is x a number?
(fn builtin_is_number ((args &Vec<LispVal>)) LispResult
    (rust {
        if args.len() != 1 {
            return Err("number?: requires exactly 1 argument".to_string());
        }
        Ok(LispVal::Bool(matches!(&args[0], LispVal::Int(_) | LispVal::Float(_))))
    }))

;; (string? x) => is x a string?
(fn builtin_is_string ((args &Vec<LispVal>)) LispResult
    (rust {
        if args.len() != 1 {
            return Err("string?: requires exactly 1 argument".to_string());
        }
        Ok(LispVal::Bool(matches!(&args[0], LispVal::Str(_))))
    }))

;; (boolean? x) => is x a boolean?
(fn builtin_is_boolean ((args &Vec<LispVal>)) LispResult
    (rust {
        if args.len() != 1 {
            return Err("boolean?: requires exactly 1 argument".to_string());
        }
        Ok(LispVal::Bool(matches!(&args[0], LispVal::Bool(_))))
    }))

;; (symbol? x) => is x a symbol?
(fn builtin_is_symbol ((args &Vec<LispVal>)) LispResult
    (rust {
        if args.len() != 1 {
            return Err("symbol?: requires exactly 1 argument".to_string());
        }
        Ok(LispVal::Bool(matches!(&args[0], LispVal::Sym(_))))
    }))

;; (pair? x) => is x a non-empty list?
(fn builtin_is_pair ((args &Vec<LispVal>)) LispResult
    (rust {
        if args.len() != 1 {
            return Err("pair?: requires exactly 1 argument".to_string());
        }
        let result = match &args[0] {
            LispVal::List(elems) => !elems.is_empty(),
            _ => false,
        };
        Ok(LispVal::Bool(result))
    }))

;; (equal? a b) => structural equality
(fn builtin_is_equal ((args &Vec<LispVal>)) LispResult
    (rust {
        if args.len() != 2 {
            return Err("equal?: requires exactly 2 arguments".to_string());
        }
        Ok(LispVal::Bool(args[0] == args[1]))
    }))

;; (zero? x) => is x zero?
(fn builtin_is_zero ((args &Vec<LispVal>)) LispResult
    (rust {
        if args.len() != 1 {
            return Err("zero?: requires exactly 1 argument".to_string());
        }
        let result = match &args[0] {
            LispVal::Int(n) => *n == 0,
            LispVal::Float(f) => *f == 0.0,
            _ => false,
        };
        Ok(LispVal::Bool(result))
    }))

;; (positive? x) => is x positive?
(fn builtin_is_positive ((args &Vec<LispVal>)) LispResult
    (rust {
        if args.len() != 1 {
            return Err("positive?: requires exactly 1 argument".to_string());
        }
        let result = match &args[0] {
            LispVal::Int(n) => *n > 0,
            LispVal::Float(f) => *f > 0.0,
            _ => false,
        };
        Ok(LispVal::Bool(result))
    }))

;; (negative? x) => is x negative?
(fn builtin_is_negative ((args &Vec<LispVal>)) LispResult
    (rust {
        if args.len() != 1 {
            return Err("negative?: requires exactly 1 argument".to_string());
        }
        let result = match &args[0] {
            LispVal::Int(n) => *n < 0,
            LispVal::Float(f) => *f < 0.0,
            _ => false,
        };
        Ok(LispVal::Bool(result))
    }))

;; (procedure? x) => is x a function?
(fn builtin_is_procedure ((args &Vec<LispVal>)) LispResult
    (rust {
        if args.len() != 1 {
            return Err("procedure?: requires exactly 1 argument".to_string());
        }
        let result = matches!(&args[0], LispVal::Lambda(_) | LispVal::Builtin(_));
        Ok(LispVal::Bool(result))
    }))

;; ============================================================
;; Section 21: Built-in String Functions
;; ============================================================

;; (string-length s) => length of string
(fn builtin_string_length ((args &Vec<LispVal>)) LispResult
    (rust {
        if args.len() != 1 {
            return Err("string-length: requires exactly 1 argument".to_string());
        }
        match &args[0] {
            LispVal::Str(s) => Ok(LispVal::Int(s.len() as i64)),
            _ => Err(format!("string-length: expected string, got: {}", args[0])),
        }
    }))

;; (string-append s1 s2 ...) => concatenate strings
(fn builtin_string_append ((args &Vec<LispVal>)) LispResult
    (rust {
        let mut result = String::new();
        for arg in args {
            match arg {
                LispVal::Str(s) => result.push_str(s),
                other => result.push_str(&format!("{}", other)),
            }
        }
        Ok(LispVal::Str(result))
    }))

;; (substring s start end) => substring
(fn builtin_substring ((args &Vec<LispVal>)) LispResult
    (rust {
        if args.len() != 3 {
            return Err("substring: requires exactly 3 arguments".to_string());
        }
        let s = match &args[0] {
            LispVal::Str(s) => s.clone(),
            _ => return Err(format!("substring: expected string, got: {}", args[0])),
        };
        let start = match &args[1] {
            LispVal::Int(n) => *n as usize,
            _ => return Err(format!("substring: expected integer start, got: {}", args[1])),
        };
        let end = match &args[2] {
            LispVal::Int(n) => *n as usize,
            _ => return Err(format!("substring: expected integer end, got: {}", args[2])),
        };
        if start > s.len() || end > s.len() || start > end {
            return Err(format!(
                "substring: indices {}-{} out of range for string of length {}",
                start, end, s.len()
            ));
        }
        let chars: Vec<char> = s.chars().collect();
        let sub: String = chars[start..end].iter().collect();
        Ok(LispVal::Str(sub))
    }))

;; (string->number s) => parse string to number
(fn builtin_string_to_number ((args &Vec<LispVal>)) LispResult
    (rust {
        if args.len() != 1 {
            return Err("string->number: requires exactly 1 argument".to_string());
        }
        match &args[0] {
            LispVal::Str(s) => {
                if let Ok(n) = s.parse::<i64>() {
                    Ok(LispVal::Int(n))
                } else if let Ok(f) = s.parse::<f64>() {
                    Ok(LispVal::Float(f))
                } else {
                    Ok(LispVal::Bool(false))
                }
            }
            _ => Err(format!("string->number: expected string, got: {}", args[0])),
        }
    }))

;; (number->string n) => convert number to string
(fn builtin_number_to_string ((args &Vec<LispVal>)) LispResult
    (rust {
        if args.len() != 1 {
            return Err("number->string: requires exactly 1 argument".to_string());
        }
        match &args[0] {
            LispVal::Int(n) => Ok(LispVal::Str(format!("{}", n))),
            LispVal::Float(f) => Ok(LispVal::Str(format!("{}", f))),
            _ => Err(format!("number->string: expected number, got: {}", args[0])),
        }
    }))

;; (string-ref s idx) => character at index as a string
(fn builtin_string_ref ((args &Vec<LispVal>)) LispResult
    (rust {
        if args.len() != 2 {
            return Err("string-ref: requires exactly 2 arguments".to_string());
        }
        let s = match &args[0] {
            LispVal::Str(s) => s.clone(),
            _ => return Err(format!("string-ref: expected string, got: {}", args[0])),
        };
        let idx = match &args[1] {
            LispVal::Int(n) => *n as usize,
            _ => return Err(format!("string-ref: expected integer, got: {}", args[1])),
        };
        let chars: Vec<char> = s.chars().collect();
        if idx < chars.len() {
            Ok(LispVal::Str(chars[idx].to_string()))
        } else {
            Err(format!(
                "string-ref: index {} out of bounds for string of length {}",
                idx, chars.len()
            ))
        }
    }))

;; (string-upcase s) => uppercase string
(fn builtin_string_upcase ((args &Vec<LispVal>)) LispResult
    (rust {
        if args.len() != 1 {
            return Err("string-upcase: requires exactly 1 argument".to_string());
        }
        match &args[0] {
            LispVal::Str(s) => Ok(LispVal::Str(s.to_uppercase())),
            _ => Err(format!("string-upcase: expected string, got: {}", args[0])),
        }
    }))

;; (string-downcase s) => lowercase string
(fn builtin_string_downcase ((args &Vec<LispVal>)) LispResult
    (rust {
        if args.len() != 1 {
            return Err("string-downcase: requires exactly 1 argument".to_string());
        }
        match &args[0] {
            LispVal::Str(s) => Ok(LispVal::Str(s.to_lowercase())),
            _ => Err(format!("string-downcase: expected string, got: {}", args[0])),
        }
    }))

;; (string-contains haystack needle) => does haystack contain needle?
(fn builtin_string_contains ((args &Vec<LispVal>)) LispResult
    (rust {
        if args.len() != 2 {
            return Err("string-contains: requires exactly 2 arguments".to_string());
        }
        let haystack = match &args[0] {
            LispVal::Str(s) => s.clone(),
            _ => return Err(format!("string-contains: expected string, got: {}", args[0])),
        };
        let needle = match &args[1] {
            LispVal::Str(s) => s.clone(),
            _ => return Err(format!("string-contains: expected string, got: {}", args[1])),
        };
        Ok(LispVal::Bool(haystack.contains(&needle)))
    }))

;; ============================================================
;; Section 22: Built-in I/O Functions
;; ============================================================

;; (display val) => print value without newline
(fn builtin_display ((args &Vec<LispVal>)) LispResult
    (rust {
        if args.len() != 1 {
            return Err("display: requires exactly 1 argument".to_string());
        }
        match &args[0] {
            LispVal::Str(s) => print!("{}", s),
            other => print!("{}", other),
        }
        let _ = std::io::stdout().flush();
        Ok(LispVal::Nil)
    }))

;; (newline) => print a newline
(fn builtin_newline ((args &Vec<LispVal>)) LispResult
    (rust {
        if !args.is_empty() {
            return Err("newline: takes no arguments".to_string());
        }
        println!();
        Ok(LispVal::Nil)
    }))

;; (print val) => print value with newline
(fn builtin_print ((args &Vec<LispVal>)) LispResult
    (rust {
        if args.len() != 1 {
            return Err("print: requires exactly 1 argument".to_string());
        }
        println!("{}", args[0]);
        Ok(LispVal::Nil)
    }))

;; (read) => read a line from stdin, parse, and return
(fn builtin_read ((args &Vec<LispVal>)) LispResult
    (rust {
        if !args.is_empty() {
            return Err("read: takes no arguments".to_string());
        }
        let mut line = String::new();
        match std::io::stdin().lock().read_line(&mut line) {
            Ok(0) => Ok(LispVal::Nil),
            Ok(_) => {
                let exprs = parse(line.trim());
                if exprs.is_empty() {
                    Ok(LispVal::Nil)
                } else {
                    Ok(exprs[0].clone())
                }
            }
            Err(e) => Err(format!("read error: {}", e)),
        }
    }))

;; ============================================================
;; Section 23: Type Conversion Builtins
;; ============================================================

;; (exact->inexact n) => convert int to float
(fn builtin_to_float ((args &Vec<LispVal>)) LispResult
    (rust {
        if args.len() != 1 {
            return Err("exact->inexact: requires exactly 1 argument".to_string());
        }
        match &args[0] {
            LispVal::Int(n) => Ok(LispVal::Float(*n as f64)),
            LispVal::Float(f) => Ok(LispVal::Float(*f)),
            _ => Err(format!("exact->inexact: expected number, got: {}", args[0])),
        }
    }))

;; (inexact->exact n) => convert float to int
(fn builtin_to_int ((args &Vec<LispVal>)) LispResult
    (rust {
        if args.len() != 1 {
            return Err("inexact->exact: requires exactly 1 argument".to_string());
        }
        match &args[0] {
            LispVal::Float(f) => Ok(LispVal::Int(*f as i64)),
            LispVal::Int(n) => Ok(LispVal::Int(*n)),
            _ => Err(format!("inexact->exact: expected number, got: {}", args[0])),
        }
    }))

;; ============================================================
;; Section 24: Utility Builtins
;; ============================================================

;; (apply func args-list) => apply function to a list of args
(fn builtin_apply ((args &Vec<LispVal>)) LispResult
    (rust {
        if args.len() != 2 {
            return Err("apply: requires exactly 2 arguments (function, list)".to_string());
        }
        let func = &args[0];
        let arg_list = match &args[1] {
            LispVal::List(elems) => elems.clone(),
            _ => return Err(format!("apply: second argument must be a list, got: {}", args[1])),
        };
        apply_func(func, &arg_list)
    }))

;; (error msg) => signal an error
(fn builtin_error ((args &Vec<LispVal>)) LispResult
    (rust {
        if args.is_empty() {
            return Err("error: user error".to_string());
        }
        let msg: String = args.iter()
            .map(|a| format!("{}", a))
            .collect::<Vec<_>>()
            .join(" ");
        Err(format!("error: {}", msg))
    }))

;; ============================================================
;; Section 25: Standard Library (Lisp definitions)
;; ============================================================
;;
;; Some standard functions defined in Lisp itself, loaded into
;; the environment at startup. These demonstrate the expressive
;; power of the interpreter.

(fn stdlib_source () &'static str
    (rust {
        r#"
;; Identity function
(define (id x) x)

;; Boolean combinators
(define (flip f) (lambda (a b) (f b a)))

;; Composition
(define (compose f g) (lambda (x) (f (g x))))

;; Numeric helpers
(define (inc x) (+ x 1))
(define (dec x) (- x 1))
(define (square x) (* x x))
(define (cube x) (* x x x))
(define (even? x) (= (modulo x 2) 0))
(define (odd? x) (not (even? x)))

;; List utilities
(define (cadr x) (car (cdr x)))
(define (caddr x) (car (cdr (cdr x))))
(define (first x) (car x))
(define (second x) (cadr x))
(define (third x) (caddr x))
(define (rest x) (cdr x))
(define (empty? x) (null? x))
(define (last lst)
  (if (null? (cdr lst))
      (car lst)
      (last (cdr lst))))

;; Higher-order helpers
(define (for-each f lst)
  (if (null? lst)
      nil
      (begin (f (car lst))
             (for-each f (cdr lst)))))

;; Fold right
(define (foldr f init lst)
  (if (null? lst)
      init
      (f (car lst) (foldr f init (cdr lst)))))

;; Zip two lists into a list of pairs
(define (zip a b)
  (if (or (null? a) (null? b))
      (list)
      (cons (list (car a) (car b))
            (zip (cdr a) (cdr b)))))

;; Take first n elements
(define (take n lst)
  (if (or (<= n 0) (null? lst))
      (list)
      (cons (car lst) (take (- n 1) (cdr lst)))))

;; Drop first n elements
(define (drop n lst)
  (if (or (<= n 0) (null? lst))
      lst
      (drop (- n 1) (cdr lst))))

;; Sum and product of a list
(define (sum lst) (reduce + 0 lst))
(define (product lst) (reduce * 1 lst))
"#
    }))

;; Load the standard library into an environment
(fn load_stdlib ((env &EnvRef)) ()
    (rust {
        let source = stdlib_source();
        let exprs = parse(source);
        for expr in exprs.iter() {
            match eval(expr, env) {
                Ok(_) => {}
                Err(e) => eprintln!("stdlib error: {}", e),
            }
        }
    }))

;; ============================================================
;; Section 26: Environment Setup
;; ============================================================
;;
;; Register all builtin functions in a fresh environment.

(fn setup_env () EnvRef
    (rust {
        let env = Env::new();

        let builtins = vec![
            // Arithmetic
            "+", "-", "*", "/", "modulo", "abs", "min", "max",
            // Comparison
            "=", "<", ">", "<=", ">=",
            // Logic
            "not",
            // List operations
            "cons", "car", "cdr", "list", "length", "append",
            "map", "filter", "reduce", "reverse", "nth", "range",
            // Predicates
            "null?", "list?", "number?", "string?", "boolean?",
            "symbol?", "pair?", "equal?", "zero?", "positive?",
            "negative?", "procedure?",
            // String operations
            "string-length", "string-append", "substring",
            "string->number", "number->string", "string-ref",
            "string-upcase", "string-downcase", "string-contains",
            // I/O
            "display", "newline", "print", "read",
            // Type conversions
            "exact->inexact", "inexact->exact",
            // Utility
            "apply", "error",
        ];

        for name in builtins {
            env.borrow_mut().define(
                name.to_string(),
                LispVal::Builtin(name.to_string()),
            );
        }

        // Define commonly used constants
        env.borrow_mut().define("nil".to_string(), LispVal::Nil);
        env.borrow_mut().define("#t".to_string(), LispVal::Bool(true));
        env.borrow_mut().define("#f".to_string(), LispVal::Bool(false));
        env.borrow_mut().define("true".to_string(), LispVal::Bool(true));
        env.borrow_mut().define("false".to_string(), LispVal::Bool(false));
        env.borrow_mut().define("pi".to_string(), LispVal::Float(std::f64::consts::PI));
        env.borrow_mut().define("e".to_string(), LispVal::Float(std::f64::consts::E));

        // Load the standard library
        load_stdlib(&env);

        env
    }))

;; ============================================================
;; Section 27: Run a Program from a String
;; ============================================================

(fn run_program ((source &str) (env &EnvRef)) LispResult
    (rust {
        let exprs = parse(source);
        let mut result = LispVal::Nil;
        for expr in exprs.iter() {
            result = eval(expr, env)?;
        }
        Ok(result)
    }))

;; ============================================================
;; Section 28: Self-Test Suite
;; ============================================================
;;
;; Runs a comprehensive suite of tests to verify the
;; interpreter works correctly. Tests cover arithmetic,
;; comparison, logic, variables, lambdas, closures, let,
;; conditionals, lists, higher-order functions, predicates,
;; strings, recursion, and the standard library.

(fn run_tests ((env &EnvRef)) ()
    (rust {
        println!("=== Lisp Interpreter Self-Test ===");
        println!();

        let tests: Vec<(&str, &str)> = vec![
            // Basic arithmetic
            ("(+ 1 2 3)", "6"),
            ("(- 10 3)", "7"),
            ("(* 2 3 4)", "24"),
            ("(/ 100 5 2)", "10"),
            ("(modulo 17 5)", "2"),
            ("(abs -42)", "42"),

            // Nested arithmetic
            ("(+ (* 3 4) (- 10 5))", "17"),
            ("(* (+ 1 2) (+ 3 4))", "21"),

            // Comparison
            ("(< 1 2)", "#t"),
            ("(> 5 3)", "#t"),
            ("(= 42 42)", "#t"),
            ("(<= 3 3)", "#t"),
            ("(>= 5 4)", "#t"),

            // Boolean logic
            ("(not #f)", "#t"),
            ("(not #t)", "#f"),
            ("(and #t #t #t)", "#t"),
            ("(and #t #f #t)", "#f"),
            ("(or #f #f #t)", "#t"),
            ("(or #f #f #f)", "#f"),

            // Variables and define
            ("(begin (define x 42) x)", "42"),
            ("(begin (define x 10) (define y 20) (+ x y))", "30"),
            ("(begin (define x 5) (set! x 99) x)", "99"),

            // Lambda and function calls
            ("((lambda (x) (* x x)) 5)", "25"),
            ("(begin (define square (lambda (x) (* x x))) (square 7))", "49"),
            ("(begin (define (add a b) (+ a b)) (add 3 4))", "7"),

            // Let expressions
            ("(let ((x 10) (y 20)) (+ x y))", "30"),
            ("(let ((x 5)) (* x x))", "25"),

            // If / cond
            ("(if #t 1 2)", "1"),
            ("(if #f 1 2)", "2"),
            ("(if (> 5 3) \"yes\" \"no\")", "\"yes\""),
            ("(cond ((= 1 2) \"a\") ((= 1 1) \"b\") (else \"c\"))", "\"b\""),

            // Quote
            ("(quote (1 2 3))", "(1 2 3)"),
            ("'(a b c)", "(a b c)"),

            // List operations
            ("(list 1 2 3)", "(1 2 3)"),
            ("(car (list 1 2 3))", "1"),
            ("(cdr (list 1 2 3))", "(2 3)"),
            ("(cons 0 (list 1 2))", "(0 1 2)"),
            ("(length (list 1 2 3 4 5))", "5"),
            ("(append (list 1 2) (list 3 4))", "(1 2 3 4)"),
            ("(reverse (list 1 2 3))", "(3 2 1)"),
            ("(nth 1 (list 10 20 30))", "20"),

            // Higher-order functions
            ("(map (lambda (x) (* x 2)) (list 1 2 3))", "(2 4 6)"),
            ("(filter (lambda (x) (> x 2)) (list 1 2 3 4 5))", "(3 4 5)"),
            ("(reduce + 0 (list 1 2 3 4 5))", "15"),

            // Predicates
            ("(null? (list))", "#t"),
            ("(null? (list 1))", "#f"),
            ("(list? (list 1 2))", "#t"),
            ("(number? 42)", "#t"),
            ("(string? \"hello\")", "#t"),
            ("(boolean? #t)", "#t"),
            ("(symbol? 'x)", "#t"),
            ("(pair? (list 1 2))", "#t"),
            ("(pair? (list))", "#f"),
            ("(zero? 0)", "#t"),
            ("(positive? 5)", "#t"),
            ("(negative? -3)", "#t"),
            ("(procedure? +)", "#t"),

            // String operations
            ("(string-length \"hello\")", "5"),
            ("(string-append \"hello\" \" \" \"world\")", "\"hello world\""),
            ("(substring \"hello world\" 0 5)", "\"hello\""),
            ("(string->number \"42\")", "42"),
            ("(number->string 42)", "\"42\""),
            ("(string-upcase \"hello\")", "\"HELLO\""),
            ("(string-downcase \"HELLO\")", "\"hello\""),
            ("(string-contains \"hello world\" \"world\")", "#t"),

            // Recursion
            ("(begin (define (fact n) (if (<= n 1) 1 (* n (fact (- n 1))))) (fact 10))", "3628800"),
            ("(begin (define (fib n) (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2))))) (fib 10))", "55"),

            // Closures
            ("(begin (define (make-adder n) (lambda (x) (+ x n))) ((make-adder 5) 10))", "15"),
            ("(begin (define (make-counter) (let ((count 0)) (lambda () (set! count (+ count 1)) count))) (let ((c (make-counter))) (begin (c) (c) (c))))", "3"),

            // Standard library functions
            ("(even? 4)", "#t"),
            ("(odd? 3)", "#t"),
            ("(square 5)", "25"),
            ("(inc 41)", "42"),
            ("(dec 43)", "42"),

            // Type conversions
            ("(exact->inexact 42)", "42"),
            ("(inexact->exact 3.7)", "3"),

            // Range and sum
            ("(sum (range 1 6))", "15"),
            ("(product (list 1 2 3 4 5))", "120"),

            // Nested let
            ("(let ((x 10)) (let ((y (+ x 5))) (+ x y)))", "25"),
        ];

        let mut passed = 0usize;
        let mut failed = 0usize;
        let total = tests.len();

        for (input, expected) in tests.iter() {
            let test_env = Env::extend(env.clone());
            match run_program(input, &test_env) {
                Ok(result) => {
                    let result_str = format!("{}", result);
                    if result_str == *expected {
                        passed += 1;
                    } else {
                        println!("  FAIL: {} => {} (expected {})", input, result_str, expected);
                        failed += 1;
                    }
                }
                Err(e) => {
                    println!("  ERROR: {} => {} (expected {})", input, e, expected);
                    failed += 1;
                }
            }
        }

        println!("Results: {} passed, {} failed, {} total", passed, failed, total);
        if failed == 0 {
            println!("All tests passed!");
        }
        println!();
    }))

;; ============================================================
;; Section 29: REPL (Read-Eval-Print Loop)
;; ============================================================
;;
;; Interactive prompt for evaluating Lisp expressions.
;; Supports multi-line input, quit/exit commands, and
;; running the test suite with the "test" command.

(fn run_repl ((env &EnvRef)) ()
    (rust {
        println!("Macro-Lisp Interpreter v1.0");
        println!("A Lisp interpreter written in macro-lisp (Lisp syntax for Rust)");
        println!("Type 'quit' or 'exit' to leave, 'test' to run self-tests");
        println!();

        let stdin = std::io::stdin();
        let mut input_buffer = String::new();

        loop {
            // Print prompt
            if input_buffer.is_empty() {
                print!("lisp> ");
            } else {
                print!("...   ");
            }
            let _ = std::io::stdout().flush();

            // Read a line
            let mut line = String::new();
            match stdin.lock().read_line(&mut line) {
                Ok(0) => {
                    println!();
                    break;
                }
                Ok(_) => {}
                Err(e) => {
                    eprintln!("Read error: {}", e);
                    break;
                }
            }

            let trimmed = line.trim();

            // Check for special commands when buffer is empty
            if input_buffer.is_empty() {
                match trimmed {
                    "quit" | "exit" => {
                        println!("Goodbye!");
                        break;
                    }
                    "test" => {
                        run_tests(env);
                        continue;
                    }
                    "" => continue,
                    _ => {}
                }
            }

            // Accumulate input for multi-line expressions
            input_buffer.push_str(&line);

            // Check if parentheses are balanced
            let open_count = input_buffer.chars().filter(|c| *c == '(').count();
            let close_count = input_buffer.chars().filter(|c| *c == ')').count();

            if open_count > close_count {
                continue;
            }

            // Evaluate the complete expression
            let source = input_buffer.trim().to_string();
            input_buffer.clear();

            if source.is_empty() {
                continue;
            }

            let exprs = parse(&source);
            for expr in exprs.iter() {
                match eval(expr, env) {
                    Ok(LispVal::Nil) => {}
                    Ok(result) => {
                        println!("=> {}", result);
                    }
                    Err(e) => {
                        eprintln!("Error: {}", e);
                    }
                }
            }
        }
    }))

;; ============================================================
;; Section 30: Demo Program
;; ============================================================
;;
;; Runs a small demonstration showcasing the interpreter.

(fn run_demo ((env &EnvRef)) ()
    (rust {
        println!("=== Lisp Interpreter Demo ===");
        println!();

        let demo_programs: Vec<(&str, &str)> = vec![
            ("Arithmetic",
             "(begin\n  (display \"2 + 3 * 4 = \")\n  (display (+ 2 (* 3 4)))\n  (newline))"),

            ("Factorial (recursive)",
             "(begin\n  (define (factorial n)\n    (if (<= n 1) 1 (* n (factorial (- n 1)))))\n  (display \"10! = \")\n  (display (factorial 10))\n  (newline))"),

            ("Fibonacci (recursive)",
             "(begin\n  (define (fib n)\n    (if (<= n 1) n\n      (+ (fib (- n 1)) (fib (- n 2)))))\n  (display \"fib(10) = \")\n  (display (fib 10))\n  (newline))"),

            ("Closures and higher-order functions",
             "(begin\n  (define (make-adder n) (lambda (x) (+ x n)))\n  (define add5 (make-adder 5))\n  (display \"add5(10) = \")\n  (display (add5 10))\n  (newline))"),

            ("Map and filter",
             "(begin\n  (define nums (list 1 2 3 4 5 6 7 8 9 10))\n  (display \"Squares of evens: \")\n  (display (map square (filter even? nums)))\n  (newline))"),

            ("FizzBuzz (1-20)",
             "(begin\n  (define (fizzbuzz n)\n    (cond\n      ((= (modulo n 15) 0) \"FizzBuzz\")\n      ((= (modulo n 3) 0) \"Fizz\")\n      ((= (modulo n 5) 0) \"Buzz\")\n      (else (number->string n))))\n  (display \"FizzBuzz: \")\n  (display (map fizzbuzz (range 1 21)))\n  (newline))"),

            ("Sum of squares (1-10)",
             "(begin\n  (display \"Sum of squares 1-10: \")\n  (display (reduce + 0 (map square (range 1 11))))\n  (newline))"),

            ("String manipulation",
             "(begin\n  (define greeting (string-append \"Hello\" \", \" \"World!\"))\n  (display \"Greeting: \")\n  (display greeting)\n  (newline)\n  (display \"Uppercase: \")\n  (display (string-upcase greeting))\n  (newline))"),

            ("Tail-recursive list builder",
             "(begin\n  (define (count-to n i)\n    (if (> i n)\n        (list)\n        (cons i (count-to n (+ i 1)))))\n  (display \"Count 1 to 10: \")\n  (display (count-to 10 1))\n  (newline))"),

            ("Quicksort",
             "(begin\n  (define (qsort lst)\n    (if (null? lst) (list)\n      (let ((pivot (car lst))\n            (rest (cdr lst)))\n        (append\n          (qsort (filter (lambda (x) (< x pivot)) rest))\n          (list pivot)\n          (qsort (filter (lambda (x) (>= x pivot)) rest))))))\n  (display \"Sort (3 1 4 1 5 9 2 6): \")\n  (display (qsort (list 3 1 4 1 5 9 2 6)))\n  (newline))"),
        ];

        for (title, code) in demo_programs.iter() {
            print!("  {} — ", title);
            let _ = std::io::stdout().flush();
            match run_program(code, env) {
                Ok(_) => {}
                Err(e) => eprintln!("Error: {}", e),
            }
        }

        println!();
    }))

;; ============================================================
;; Section 31: Main Entry Point
;; ============================================================
;;
;; Sets up the environment, runs the self-test, runs the demo,
;; then starts the REPL.

(fn main () ()
    (rust {
        let env = setup_env();

        let args: Vec<String> = std::env::args().collect();

        if args.len() > 1 {
            match args[1].as_str() {
                "--test" => {
                    run_tests(&env);
                }
                "--demo" => {
                    run_demo(&env);
                }
                "--repl" => {
                    run_repl(&env);
                }
                filename => {
                    match std::fs::read_to_string(filename) {
                        Ok(source) => {
                            match run_program(&source, &env) {
                                Ok(_) => {}
                                Err(e) => {
                                    eprintln!("Error: {}", e);
                                    std::process::exit(1);
                                }
                            }
                        }
                        Err(e) => {
                            eprintln!("Cannot read file '{}': {}", filename, e);
                            std::process::exit(1);
                        }
                    }
                }
            }
        } else {
            // Default: run tests, demo, then REPL
            run_tests(&env);
            run_demo(&env);
            run_repl(&env);
        }
    }))
