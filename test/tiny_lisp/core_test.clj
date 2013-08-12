(ns tiny-lisp.core-test
  (:use clojure.test
        tiny-lisp.core))

(defn fuzzy= [x y]
  (< (Math/abs (- x y)) 0.000001))

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
  (testing "Multiple root lists, should return all"
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
    (is (= (parse-list [")"] 0 '()) ['(), 1])))
  (testing "Single item case"
    (is (= (parse-list ["42" ")"] 0 '()) ['(42), 2])))
  (testing "Multiple items"
    (is (= (parse-list ["1" "2" "3" ")"] 0 '()) ['(1 2 3), 4])))
  (testing "Partial list case"
    (is (= (parse-list ["1" "2" "3" ")" "4" "5"] 0 '()) ['(1 2 3), 4])))
  (testing "Partial nested list case"
    (is (= (parse-list ["1" "2" "3" "(" "4" "5" ")" "6" ")" "7"] 0 '()) ['(1 2 3 (4 5) 6), 9])))
  (testing "Missing close bracket case"
    (is (thrown? IllegalArgumentException (parse-list ["1" "2" "3"] 0 '()))))
  )

