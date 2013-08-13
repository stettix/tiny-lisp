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

    ; TODO: See if I can do all this destruturing inside the [[ ]].    
    [[ "atom" & args ]] (let [[arg1 arg2] args
                          [res newEnv] (eval-exp arg1 env)]
                          (if arg2 (throw (IllegalArgumentException. "Exactly one argument excpected for 'atom'")))
                          [(not (coll? res)), newEnv])
    [[ "eq?" & args ]] (let [[arg1 arg2] args]
                         ; TODO: Shouldn't these be evalled?
                         [(= arg1 arg2) env])
    [[ "null?" & args ]] (let [[arg1 arg2] args
                               [res newEnv] (eval-exp arg1 env)]
                           (if arg2 (throw (IllegalArgumentException. "Exactly one argument excpected for 'null?'")))
                            [(= [] res) newEnv])
    [[ "car" & args ]] (let [[arg1 arg2] args
                             [res newEnv] (eval-exp arg1 env)]
                         (if arg2 (throw (IllegalArgumentException. "Exactly one argument excpected for 'car'")))
                         [(first res) newEnv])
    [[ "cdr" & args ]] (let [[arg1 arg2] args
                             [res newEnv] (eval-exp arg1 env)]
                         (if arg2 (throw (IllegalArgumentException. "Exactly one argument excpected for 'cdr'")))
                         [(rest res) newEnv])
    [[ "cons" & args ]] (let [[arg1 arg2 arg3] args
                              [res1 newEnv] (eval-exp arg1 env)
                              [res2 newEnv2] (eval-exp arg2 newEnv)]
                          (if (not (coll? res2)) (throw (IllegalArgumentException. "Second argument to 'cons' must be a list")))
                          (if arg3 (throw (IllegalArgumentException. "Exactly two arguments excpected for 'cdr'")))
                          [(cons res1 res2) env])
    [[ "define" & args ]] (let [[sym arg arg2] args
                                [res newEnv] (eval-exp arg env)]
                            (if arg2 (throw (IllegalArgumentException. "Exactly two arguments excpected for 'define'")))
                            [sym (conj newEnv [sym res])])
    ; TODO: Add a test that chains defines and/or set!s.
    [[ "set!" & args ]] (let [[sym arg arg2] args
                              [res newEnv] (eval-exp arg env)]
                          (if arg2 (throw (IllegalArgumentException. "Exactly two arguments excpected for 'set!'")))
                          (let [prevValue (get env sym)]
                            (println ">> " prevValue " : " env)
                            (if (not prevValue) (throw (IllegalArgumentException. (str "Uknown symbol '" sym "'"))))
                            [prevValue (conj newEnv [sym res])]))
                            
;    [["begin" & args]] (let [results (map #(eval-exp %1 env) args)]
;                         (last results))
     [["begin" & args]] (loop [exprs args
                               previousEnv env]
                          (let [[res updatedEnv] (eval-exp (first expr) previousEnv)
                               remainingArgs (rest args)]
                            (if (empty? remainingArgs) 
                              [res updatedEnv]
                              (recur (rest args) updatedEnv))))

    ;[(last (map #(eval-exp %1 env) args)) env] ; TODO: Should get last env, but how?
    ; Actually MUST do this, or (being (define) (set)) won't work!!!
    
    ; TODO: Add an arg2 to the above and throw exception if it's non-nil?
    
    ; TODO: have a block for all cases that take args that need to be evaluated, and evaluate them 
    ; all together before proceeding?
    
    :else [expr env]))

(eval-exp ["begin" 1 2 3] {})
(eval-exp ["define" "x" 42] {})
(eval-exp ["begin" ["define" "x" 42]] {})
(eval-exp ["begin" ["define" "x" 42] ["set!" "x" 36]] {})

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
