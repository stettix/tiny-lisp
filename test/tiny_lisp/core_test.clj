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

