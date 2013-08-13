(ns tiny-lisp.core
  (require [clojure.string :as string])
  (use [clojure.core.match :only (match)])
  (import (java.lang IllegalArgumentException)))

(defmacro try-or
  "See http://clj-me.cgrand.net/2009/01/08/try-or-or-try-try-else-or-else-try/" 
  ([] nil)
  ([form] form)
  ([form & forms]
    `(try 
       ~form
       (catch Exception e#
         (try-or ~@forms)))))

(defn create-default-env []
  ; TODO: include various default operators in the returned map.
  {})

; [single expression, environment map] -> [result of evaluation, updated environment map]
(defn eval-exp [expr env]
  (match [expr]
    [(s :guard string?)] [(get env expr) env]
    [[ (:or "q" "quote") & args ]] [(first args) env]
    [[ "atom" & args ]] [(not (coll? (first (eval-exp (first args) env)))) env] ;TODO: destructure?
    [[ "eq?" & args ]] (let [[arg1 arg2] args]
                         ; TODO: Shouldn't these be evalled?
                         [(= arg1 arg2) env])
    [[ "null?" & args ]] (let [[arg1] args
                               [res newEnv] (eval-exp arg1 env)]
                            [(= [] res) newEnv])
    [[ "car" & args ]] (let [[arg1 arg2] args
                             [res newEnv] (eval-exp arg1 env)]
                         (if arg2 (throw (IllegalArgumentException. "Exactly one argument excpected for 'car'")))
                         [(first res) newEnv])
    [[ "cdr" & args ]] (let [[arg1 arg2] args
                             [res newEnv] (eval-exp arg1 env)]
                         (println arg1 " -> " res)
                         (if arg2 (throw (IllegalArgumentException. "Exactly one argument excpected for 'cdr'")))
                         [(rest res) newEnv])
                             
    ; TODO: Add an arg2 to the above and throw exception if it's non-nil?
    
    ; TODO: have a block for all cases that take args that need to be evaluated, and evaluate them 
    ; all together before proceeding?
    
    :else [expr env]))

(eval-exp ["cdr" '(1 2 3)] {})

(defn parse-atom [str]
  (cond 
    (= "true" str) true
    (= "false" str) false
    :else (try-or 
            (Long/parseLong str) 
            (Double/parseDouble str) 
            str)))

; list of token strings -> tuple of [parsed list, # of read tokens]
(defn parse-list [tokens usedTokenCount aggr]
  (cond
    (empty? tokens) (throw (IllegalArgumentException. "Missing ')'"))
    (= ")" (first tokens)) [aggr (inc usedTokenCount)]
    (= "(" (first tokens)) (do
                             (let [[nestedList, consumedTokens]
                               (parse-list (rest tokens) 1 [])]
                               (parse-list 
                                 (drop consumedTokens tokens) 
                                 (+ usedTokenCount consumedTokens) 
                                 (conj aggr nestedList))))
    :else (recur (rest tokens) (inc usedTokenCount) (conj aggr (parse-atom (first tokens))))
    ))

; list of tokens -> list of parsed expressions.
(defn parse [tokens]
  (get (parse-list (concat tokens [")"]) 0 []) 0))

(defn tokenize [str]
  (remove #(string/blank? %) 
          (string/split (string/replace str #"([\(\)])" " $0 ") #"\s")))

(defn expr->string [expr] 
  (if (seq? expr) 
    (str "(" (string/join " " expr) ")")
    (.toString expr)))

(defn repl []
  (println "Welcome to tiny-lisp!")
  ; TODO: Provide a prompt for each line...
  (doseq [line (line-seq (java.io.BufferedReader. *in*))]
    (println (expr->string (eval-exp (parse (tokenize line)))))
    ))

;  (repl)
