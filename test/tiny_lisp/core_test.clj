(ns tiny-lisp.core-test
  (:use clojure.test)
  (:use tiny-lisp.core))

(defn fuzzy= [x y]
  (< (Math/abs (- x y)) 0.000001))

(def env (create-default-env))

(deftest test-tokenize
  (testing "tokenize empty string"
    (is (= (tokenize "") '())))
  (testing "tokenize single token"
    (is (= (tokenize "42") '("42"))))
  (testing "tokenize single token with floating point value"
    (is (= (tokenize "42.0") '("42.0"))))
  (testing "tokenize single empty list"
    (is (= (tokenize "()") '("(" ")"))))
  (testing "tokenize single list with one item"
    (is (= (tokenize "(1)") '("(" "1" ")"))))
  (testing "tokenize single list with multiple items"
    (is (= (tokenize "(1 42.0 3)") '("(" "1" "42.0" "3" ")"))))
  (testing "tokenize nested lists"
    (is (= (tokenize "(* (+ 2 3) 1)") '("(" "*" "(" "+" "2" "3" ")" "1" ")"))))
  )

(deftest parse-simple-values
  (testing "Parse long"
    (is (= (parse-atom "42") 42)))
  (testing "Parse double"
    (is (fuzzy= (parse-atom "42.1") 42.1)))
  (testing "Parse 'true'"
    (is (parse-atom "true")))
  (testing "Parse 'false'"
    (is (not (parse-atom "false"))))
  (testing "Parse number"
    (is (= (parse-atom "42") 42)))
  (testing "Parse string"
    (is (= (parse-atom "foo") "foo")))
  )

(deftest test-expr->string
  (testing "Convert number"
    (is (= (expr->string 42) "42")))
  (testing "Convert string"
    (is (= (expr->string "str") "str")))
  (testing "Convert boolean 'true'"
    (is (= (expr->string true) "true")))
  (testing "Convert boolean 'false;"
    (is (= (expr->string false) "false")))
  (testing "Convert empty list"
    (is (= (expr->string '()) "()")))
  (testing "Convert single entry list"
    (is (= (expr->string '(42)) "(42)")))
  (testing "Convert multiple entry list"
    (is (= (expr->string '(42 3.14 256)) "(42 3.14 256)")))
  )

(deftest test-parse-simple-cases
  (testing "Parse empty string"
    (is (= (parse '("")) '(""))))
  (testing "Parse single integer"
    (is (= (parse '("42")) '(42))))
  (testing "Parse single float"
    (is (fuzzy= (first (parse '("1.234"))) 1.234)))
  (testing "Parse true"
    (is (= (parse '("true")) '(true))))
  (testing "Parse false"
    (is (= (parse '("false")) '(false))))
  (testing "Parse symbol"
    (is (= (parse '("xyz")) '("xyz"))))
  (testing "Parse multiple tokens"
    (is (= (parse '("+" "1" "2")) '("+" 1 2))))
)

(deftest test-parse-lists
  (testing "Parse empty list"
    (is (= (parse '("(" ")")) '(()))))
  (testing "Parse simple list"
    (is (= (parse '("(" "+" "1" "2" ")")) 
           '(("+" 1 2)))))
  (testing "Parse nested lists"
    (is (= (parse '("(" "+" "1" "(" "+" "2" "3" ")" ")"))
           '(("+" 1 ("+" 2 3)))))))

(deftest test-parse-multiple-lists
  (testing "Multiple root lists should return all"
    (is (= (parse '("(" "+" "1" "2" ")" "3" "(" "4" "5" ")")) 
           '(("+" 1 2) 3 (4 5))))))

(deftest test-parse-error-cases
  (testing "Parse open bracket only"
    (is (thrown? IllegalArgumentException (parse '("(")))))
  (testing "Parse missing close bracket"
    (is (thrown? IllegalArgumentException (parse '("(" "+" "1" "2")))))
  )

(deftest test-parse-list
  (testing "Empty list case"
    (is (= (parse-list [")"] 0 '()) ['() 1])))
  (testing "Single item case"
    (is (= (parse-list ["42" ")"] 0 '()) ['(42) 2])))
  (testing "Multiple items"
    (is (= (parse-list ["1" "2" "3" ")"] 0 '()) ['(1 2 3) 4])))
  (testing "Partial list case"
    (is (= (parse-list ["1" "2" "3" ")" "4" "5"] 0 '()) ['(1 2 3) 4])))
  (testing "Partial nested list case"
    (is (= (parse-list ["1" "2" "3" "(" "4" "5" ")" "6" ")" "7"] 0 '()) ['(1 2 3 (4 5) 6) 9])))
  (testing "Missing close bracket case"
    (is (thrown? IllegalArgumentException (parse-list ["1" "2" "3"] 0 '()))))
  )

(defn eval-str [str]
  (eval-exp (get (parse (tokenize str)) 0) env))

(deftest access-unknown-symbols
  (is (thrown? Exception (eval-str "x")))
  (is (thrown? Exception (eval-str "(begin (define x 42) y)")))
  )

(deftest eval-with-literal-argument
  (testing  "eval with numeric literal"
    (is (= (eval-str "42") 42)))
  (testing  "eval with boolean value true"
    (is (eval-str "true")))
  (testing "eval with boolean value false"
    (is (eval-str "false")))
  )

(deftest eval-quote
  (testing "eval string with quoted numeric literal"
    (is (= (eval-str "(q 42)") 42)))
  (testing "eval string with quoted symbol"
    (is (= (eval-str "(q foo)") "foo" )))
  (testing "eval string with quoted empty list"
    (is (= (eval-str "(q ())") '() )))
  (testing "eval string with quoted list of numeric literals"
    (is (= (eval-str "(q (1 2 3))") '(1 2 3) )))
  (testing "eval string with quoted list of numeric literals using long form of quote"
    (is (= (eval-str "(quote (1 2 3))") '(1 2 3) )))
  (testing "eval quote with nested lists"
    (is (= (eval-str "(quote ((1 2) (3 4)))") '('(1 2) '(3 4)))))
  (testing "eval quote with deeply nested lists"
    (is (= (eval-str "(quote ((1 2) (3 4) ((5 6) (7 8))))") '('(1 2) '(3 4) '('(5 6) '(7 8))))))
  )

(deftest eval-quote-invalid-arguments
  (is (thrown? IllegalArgumentException (eval-str "(quote)")))
  (is (thrown? IllegalArgumentException (eval-str "(quote 1 2)")))
  )

(deftest eval-atom
  (testing "integer literal is an atom"
    (is (eval-str "(atom 42)")))
  (testing "float literal is an atom"
    (is (eval-str "(atom 3.14)")))
  (testing "boolean literal is an atom"
    (is (eval-str "(atom false)")))
  (testing "empty list is an atom"
    (is (eval-str "(atom (q ()))")))
  (testing "non-empty list is not an atom"
    (is (not (eval-str "(atom (q (1 2 3)))")))))
    
(deftest eval-atom-invalid-cases
  (is (thrown? IllegalArgumentException (eval-str "(atom)")))
  (is (thrown? IllegalArgumentException (eval-str "(atom 21 42)"))))

(deftest eval-eq?
  (testing "eq? with two equal numbers"
    (is (eval-str "(eq? 42 42)")))
  (testing "eq? with two different numbers"
    (is (not (eval-str "(eq? 42 123)"))))
  (testing "eq? with two empty lists"
    (is (eval-str "(eq? (quote ()) (quote ()))")))
  (testing "eq? with non-empty and empty list"
    (is (not (eval-str "(eq? (quote (a b)) (quote ()))"))))
  (testing   "eq? with empty and non-empty list"
    (is (not (eval-str "(eq? (quote ()) (quote (a b)))"))))
  (testing "eq? with two non-empty lists should return false even if lists have same entries"
    (is (not (eval-str "(eq? (quote (a b)) (quote (a b)))"))))
  (testing "eq? should evaluate arguments"
    (is (eval-str "(eq? (+ 1 2) (+ 2 1))"))))

  (deftest eval-eq?-invalid-arguments
    (is (thrown? IllegalArgumentException (eval-str "(eq?)")))
    (is (thrown? IllegalArgumentException (eval-str "(eq? 1)")))
    (is (thrown? IllegalArgumentException (eval-str "(eq? 1 2 3)")))
    (is (thrown? IllegalArgumentException (eval-str "(eq? 42 123 foo)")))
    (is (thrown? IllegalArgumentException (eval-str "(eq? (quote ()) (quote ()) 42)"))))

  (deftest eval-car-with-non-list
    (is (thrown? IllegalArgumentException (eval-str "(car 42)"))))

  (deftest eval-car-with-empty-list
    (is (thrown? IllegalArgumentException (eval-str "(car (quote ()))")))

  (deftest eval-car-valid-cases
    (testing "car with single-item list"
      (is (= (eval-str "(car (quote (1)))") 1)))
    (testing "car with multi-item list"
      (is (= (eval-str "(car (quote (1 2 3)))") 1))))

  (deftest eval-cdr-with-non-list
    (is (thrown? IllegalArgumentException (eval-str "(cdr 42)"))))

  (deftest eval-cdr-with-empty-list
    (is (thrown? IllegalArgumentException (eval-str "(cdr (quote ()))"))))

  (deftest eval-cdr-valid-cases
    (testing  "cdr with single-item list"
      (is (= (eval-str "(cdr (quote (1)))") '())))
    (testing  "cdr with multi-item list"
      (is (= (eval-str "(cdr (quote (1 2 3)))") '(2 3))))
    (testing  "cdr with nested list"
      (is (= (eval-str "(cdr (quote ((1 2) (3 4))))") '('(3 4)))))
    (testing "cdr with single-item list should ignore extra arguments"
      (is (= (eval-str "(cdr (quote (1)) 1)") '())))))

  (deftest eval-null?
    (testing "null? with single integer literal"
      (is (not (eval-str "(null? 0)"))))
    (testing "null? with single boolean literal"
      (is (not (eval-str "(null? false)"))))
    (testing "null? with single literal 42"
      (is (not (eval-str "(null? 42)"))))
    (testing "null? with non-empty list argument"
      (is (not (eval-str "(null? (quote (1 2 3)))"))))
    (testing "null? with empty list argument"
      (is (= (eval-str "(null? (quote ()))")))))

  (deftest eval-null?-with-missing-argument
    (is (thrown? IllegalArgumentException (eval-str "(null?)"))))

  (deftest eval-null?-with-too-many-arguments
    (is (thrown? IllegalArgumentException (eval-str "(null? 1 2)"))))

  (deftest eval-cons-valid-cases
    (testing "cons value to empty list"
      (is (= (eval-str "(cons 1 (quote ()))") '(1))))
    (testing "cons value to list with single item"
      (is (= (eval-str "(cons 1 (quote (2)))") '(1 2))))
    (testing "cons value to list with multiple items"
      (is (= (eval-str "(cons 1 (quote (2 3 4)))") '(1 2 3 4))))
    (testing "cons list to list with multiple items"
      (is (= (eval-str "(cons (quote (1)) (quote (2 3 4)))") '('(1) 2 3 4))))
  )

  (deftest eval-cons-invalid-arguments
    (is (thrown? IllegalArgumentException (eval-str "(cons)")))
    (is (thrown? IllegalArgumentException (eval-str "(cons 1)")))
    (is (thrown? IllegalArgumentException (eval-str "(cons 1 2)")))
  )

  (deftest eval-if-valid-cases
    (testing "'if' with literal 'true' value"
      (is (= (eval-str "(if true 42 36)") 42)))
    (testing "'if' with literal 'false' value"
      (is (= (eval-str "(if false 42 36)") 36)))
    (testing "'if' with 1 literal"
      (is (= (eval-str "(if 1 42 36)") 42)))
    (testing "'if' with 0 literal"
      (is (= (eval-str "(if 0 42 36)") 42)))
    (testing "'if' with list value"
      (is (= (eval-str "(if (quote ()) 42 36)") 42)))

    (testing "'if' with evaluted consequence"
      (is (= (eval-str "(if true (quote (1 2)) 0)") '(1 2))))
    (testing "'if' with evaluted alternative"
      (is (= (eval-str "(if false 0 (quote (1 2)))") '(1 2))))

    (testing "'if' with calculated false predicate"
      (is (= (eval-str "(if (< 1 2) 42 36)") 42)))
    (testing "'if' with calculated true predicate"
      (is (= (eval-str "(if (> 1 2) 42 36)") 36))))

  (deftest eval-if-invalid-arguments
    (is (thrown? IllegalArgumentException (eval-str "(if)")))
    (is (thrown? IllegalArgumentException (eval-str "(if false)")))
    (is (thrown? IllegalArgumentException (eval-str "(if false 42)")))
    (is (thrown? Exception (eval-str "(if false 42 36 666)"))))

  (deftest eval-cond-valid-cases
    (testing  "'cond' with first condition matching"
      (is (= (eval-str "(cond (true 42) (false 36))") 42)))
    (testing "'cond' with second condition matching"
      (is (= (eval-str "(cond (false 42) (true 36))") 36)))
    (testing "'cond' with calculated predicate"
      (is (= (eval-str "(cond ((< 1 2) 42))") 42)))
    (testing "'cond' with calculated predicates"
      (is (= (eval-str "(cond ((> 1 2) 36) ((< 1 2) 42))") 42)))
    (testing "'cond' with calculated result expression"
      (is (= (eval-str "(cond (true (* 2 21)))") 42))))

  (deftest eval-cond-no-valid-condition-matching
    (is (thrown? Exception (eval-str "(cond (false 42))")))
    (is (thrown? Exception (eval-str "(cond (false 42) (false 36))"))))

  (deftest eval-cond-invalid-arguments
    (is (thrown? Exception (eval-str "(cond)")))
    (is (thrown? IllegalArgumentException (eval-str "(cond (true))")))
    (is (thrown? IllegalArgumentException (eval-str "(cond (false 42) (true))")))
  )

  (deftest eval-begin-simple-cases
    (testing "single expression with literal value"
      (is (= (eval-str "(begin 1)") 1)))
    (testing "multiple expressions with literal values"
      (is (= (eval-str "(begin 1 2 3)") 3)))
    (testing "multiple expressions with list values"
      (is (= (eval-str "(begin (quote (1 2)) (quote (3 4)))") '(3 4))))
  )

  (deftest eval-begin-error-cases
    (is (thrown? Exception (eval-str "(begin)"))))

  (deftest eval-define-check-returned-value
    ; This behaviour differs from tiddlylisp; a (define) statement there returns nothing.
    ; We match the behaviour of MIT/GNU Scheme instead.
    (testing "simple define expression should return the defined symbol"
      (is (= (eval-str "(define x 42)") "x" ))))

  (deftest eval-define-invalid-arguments
    (is (thrown? IllegalArgumentException (eval-str "(define)")))
    (is (thrown? IllegalArgumentException (eval-str "(define x)")))
    (is (thrown? IllegalArgumentException (eval-str "(define 1 2)")))
    (is (thrown? Exception (eval-str "(define x 1 2 3)"))))

  (deftest eval-set!-check-returned-value
    ; As for 'define' this behaviour differs from tiddlylisp a 'set!' expression there returns nothing.
    (testing "simple set! expression should return the previous value"
      (is (= (eval-str "(begin (define x 42) (set! x 36))") 42))))

  (deftest eval-set!-for-undefined-symbol
    (is (thrown? IllegalArgumentException (eval-str "(set! x 42)"))))

  (deftest eval-set!-invalid-arguments
    (is (thrown? IllegalArgumentException (eval-str "(set!)")))
    (is (thrown? IllegalArgumentException (eval-str "(set! x)")))
    (is (thrown? IllegalArgumentException (eval-str "(set! x 1 2)"))))

  (deftest eval-define-set!-and-symbol-lookup
    (testing "define then set"
      (is (= (eval-str "(begin (define x 42) (set! x 36) x)") 36)))
    (testing "define twice should overwrite value"
      (is (= (eval-str "(begin (define x 42) (define x 36) x)") 36)))
    (testing "Define for lambda should return its name"
      (is (= (eval-str "(begin (define id (lambda (x) x)))") "id")))
    (testing "Defined ID function should return same value"
      (is (= (eval-str "(begin (define id (lambda (x) x)) (id 1))") 1))))

  (deftest eval-lambda-simple-cases
    (def l (eval-str "(lambda () 42)"))
    (testing "Lambda with no parameters return number called on returned closure"
      (is (= (l '()) 42)))
    (testing "Lambda with no parameters return number"
      (is (= (eval-str "((lambda () 42))") 42)))
    (testing "Lambda with single parameter returns that parameter (number)"
      (is (= (eval-str "((lambda (x) x) 42)") 42)))
    (testing "Lambda with single parameter returns that parameter (list)"
      (is (= (eval-str "((lambda (x) x) (quote (1 2 3)))") '(1 2 3)))))

  (deftest eval-lambda-invalid-numbers-of-arguments
    (testing "Missing argument"
      (is (thrown? Exception (eval-str "((lambda (x) x))"))))

    ; Could add checks for too many arguments
    ; (is (thrown? Exception (eval-str "((lambda () 42) 123)") )
    ; (is (thrown? Exception (eval-str "((lambda (x) x) 123 456)") )
  )

  (deftest eval-lambda-with-procedure-calls
    (testing  "Lambda with single parameter returns square of parameter"
      (is (= (eval-str "((lambda (x) (* x x)) 4))") 16)))
    (testing  "Lambda assigned to symbol then used"
      (is (= (eval-str "(begin (define square (lambda (x) (* x x))) (square 4))") 16))))

  (deftest eval-unknown-procedure-call
    (is (thrown? IllegalArgumentException (eval-str "(foo)")))
    (is (thrown? IllegalArgumentException (eval-str "(foo 1 2 3)"))))

  (deftest symbol-defined-in-nested-environment
    (def code "(begin
                 (define x 1)
                 (define foo (lambda (x) (* x 2)))
                 (+ x (foo 3)))")
    (testing "Argument to lambda is different from variable in outer environment"
      (is (= (eval-str code) 7)))
    
    (def code2 "(begin
                  (define x 1)
                  (define foo (lambda (y) (begin (define x 2) (* x y))))
                  (+ x (foo 3)))")
    (testing "Variable defined inside lambda is different from variable in outer environment"
      (is (= (eval-str code2) 7)))
  )

  (defn evalMultipleStr [str]
    (def exprs (parse (tokenize str)))
    (map #(eval % env) exprs))

  (deftest eval-program-of-multiple-expressions
    (testing "Multiple literal expressions on one line"
      (is (= (evalMultipleStr("1 2 3") '(1 2 3))))))

  (deftest recurisve-factorial-function
    (def code "(begin
                  (define factorial (lambda (n) (if (<= n 1) 1 (* n (factorial (- n 1))))))
                  (factorial 6))")
    (is (= (eval-str code) 720)))

  (deftest square-roots-by-newton's-method
    (def code "(begin
                 (define sqrt (lambda (x) (sqrt-iter 1 x)))
					       (define sqrt-iter (lambda (guess x) (if (good-enough? guess x) guess (sqrt-iter (improve guess x) x))))
	                  (define good-enough? (lambda (guess x) (< (abs (- x (square guess))) 0.00001)))
	                  (define abs (lambda (x) (if (< 0 x) x (- 0 x))))
	                  (define square (lambda (x) (* x x)))
	                  (define improve (lambda (guess x) (average guess (/ x guess))))
	                  (define average (lambda (x y) (* 0.5 (+ x y))))
	                  (sqrt 2))")
    (def squareRootOfTwo (eval-str(code)))
    (is (< (- (java.lang.Math/abs squareRootOfTwo) 1.41421)) 0.0001))
