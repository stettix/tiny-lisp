(ns tiny-lisp.core
  (require [clojure.string :as string])
  (use [clojure.core.match :only (match)])
  (import (java.lang IllegalArgumentException))
  (:gen-class))

; We define an environment as a function that returns the value of a named symbol.
; Maps work nicely as such functions.
; The default environment contains a basic set of operations.
(def default-env 
  { "+" +, "-" -, "*" *, "/" /, "<" <, "<=" <=, ">" >, ">=" >=, "=" = })

; Return an environment that includes the new value. 
(defn env-set [env symbol value]
  (fn [sym]
    (if (= sym symbol) 
      value
      (env sym))))

; Return an environment that will perform lookups in the 'inner' environment first,
; then look it up in the outer if no result was found.
(defn wrap-env [inner-env outer-env]
  (fn [sym] 
    (or (inner-env sym) (outer-env sym))))

(defn error [message]
  (throw (IllegalArgumentException. message)))

(defn error-if [has-error message]
  (if has-error (error message)))

(defn is-truthy [val]
  (not (= false val)))

(declare eval-exprs define-lambda)

; [expression, environment] -> [result of evaluation, updated environment]
(defn eval-exp [expr env]
  (match [expr]
    [(s :guard string?)] (if (env s)
                           [(env s) env]
                           (error (str "Unknown symbol '" s "'")))
    
    [[(:or "q" "quote") & args]] (let [[arg1 arg2] args
                                         _ (error-if (or (nil? arg1) arg2) "Exactly one argument excpected for 'quote'")]
                                     [(first args) env])
    
    [["atom" & args]] (let [[arg1 arg2] args
                              _ (error-if (or (nil? arg1) arg2) "Exactly one argument excpected for 'atom'")
                              [res newEnv] (eval-exp arg1 env)]
                          [(not (coll? res)), newEnv])
    
    [["eq?" & args]] (let [[arg1 arg2 arg3] args
                             _ (error-if (or (nil? arg1) (nil? arg2) arg3) "Exactly two arguments excpected for 'eq?'")
                             [res1 env1] (eval-exp arg1 env)
                             [res2 env2] (eval-exp arg2 env1)]
                           [(= res1 res2) env2])
    
    [["null?" & args]] (let [[arg1 arg2] args
                               _ (error-if (or (nil? arg1) arg2) "Exactly one argument excpected for 'null?'")
                               [res newEnv] (eval-exp arg1 env)]
                           [(= [] res) newEnv])
    
    [["car" & args]] (let [[arg1 arg2] args
                             _ (error-if (or (nil? arg1) arg2) "Exactly one argument excpected for 'car'")
                             [res newEnv] (eval-exp arg1 env)
                             _ (error-if (empty? res) "Argument to 'car' must be a non-empty list")]
                         [(first res) newEnv])
    
    [["cdr" & args]] (let [[arg1 arg2] args
                             _ (error-if (or (nil? arg1) arg2) "Exactly one argument excpected for 'cdr'")
                             [res newEnv] (eval-exp arg1 env)
                             _ (error-if (empty? res) "Argument to 'cdr' must be a non-empty list")]
                         [(rest res) newEnv])
    
    [["cons" & args]] (let [[arg1 arg2 arg3] args
                              _ (error-if arg3 "Exactly two arguments excpected for 'cons'")
                              [res1 _] (eval-exp arg1 env)
                              [res2 _] (eval-exp arg2 env)]
                        (error-if (not (coll? res2)) "Second argument to 'cons' must be a list")
                        [(cons res1 res2) env])
    
    [["define" & args]] (let [[sym arg rest] args
                              _ (error-if (or (not (string? sym)) (nil? arg) rest) 
                                  "Exactly two arguments excpected for 'define'")
                              [res newEnv] (eval-exp arg env)]
                          [sym (env-set newEnv sym res)])
    
    [["set!" & args]] (let [[sym arg arg3] args
                            _ (error-if arg3 "Exactly two arguments excpected for 'set!'")
                            [res newEnv] (eval-exp arg env)]
                        (let [prevValue (env sym)]
                          (error-if (not prevValue) (str "Unknown symbol '" sym "'"))
                          [prevValue (env-set newEnv sym res)]))
                            
    [["begin" & args]]  (let [_ (error-if (empty? args) "Must provide at least one expression for 'begin'")
                              [results new-env] (eval-exprs args env)]
                              [(last results) new-env])
     
    [["if" & args]] (let [[test-exp conseq alt rest] args
                          _ (error-if (or (nil? test-exp) (nil? conseq) (nil? alt) rest) "Expected three arguments for 'if'")
                          [test-result newEnv] (eval-exp test-exp env)]
                          (if (is-truthy test-result)
                            (eval-exp conseq newEnv) 
                            (eval-exp alt newEnv)))
    
    [["cond" & args]] (let [_ (if (= 1 (mod (count args) 2)) (error "Odd number of arguments to 'cond'"))  
                            cond-exprs (apply array-map args)]
                        (loop [remaining-exprs cond-exprs]
                          (if (empty? remaining-exprs)
                            (error (str "None of the conditions matched: " expr))
                            (let [[cnd conseq] (first remaining-exprs)
                                  [cnd-res _] (eval-exp cnd env)]
                              (if (is-truthy cnd-res)
                                (let [[res new-env] (eval-exp conseq env)]
                                  [res new-env])
                                (recur (rest remaining-exprs)))))))
    
    [["lambda" & args]] (let [[arg-names lambda-expr rest] args
                              _ (error-if (or (nil? arg-names) (nil? lambda-expr) rest) "Expected two arguments for 'lambda'")
                              lambda (define-lambda arg-names lambda-expr env)
                              ]
                          [lambda env])

    [["defn" & args]] (let [[name arg-names fun-expr rest] args
                            _ (error-if (or (not (string? name)) (not fun-expr) rest)
                                          "Exactly three arguments excpected for 'defn'")
                            
                            ; Define mutable reference to lambda function itself,
                            ; to enable recursive calls.
                            lambda-ref (atom nil)
                            env-lookup (fn [symbol]
                                         (if (= symbol name)
                                           @lambda-ref
                                           (env symbol)))
                                           
                            lambda (define-lambda arg-names fun-expr env-lookup)]
                        (reset! lambda-ref lambda)
                        [lambda env-lookup])
    
    :else (if (coll? expr)
            ;; Evaluate procedure call.
            (let [evalled-args (map #(first (eval-exp % env)) expr)
                  proc (first evalled-args)
                  proc-args (rest evalled-args)
                  result (apply proc proc-args)]
              [result env])
            ;; Simple value evaluates as itself.
            [expr env])
    ))

; Returns a function that takes the given arguments and evaluates the given expression.
(defn define-lambda [arg-names lambda-expr env]
  (fn [& arg-values]
    (let [args-env (zipmap arg-names arg-values)
          new-env (wrap-env args-env env)]
      (first (eval-exp lambda-expr new-env)))))

; Evaluates all given expressions, chaining the environment returned by each evaluation
; through to the next one. Returns [list of results of the evaluation, final environment]
; It's basically a 'map' operation - except we have to pass the updated environment along. 
 (defn eval-exprs [exprs env]
  (loop [es exprs
         previous-env env
         results []]
    (if (empty? es)
      [results previous-env]
      (let [[res updatedEnv] (eval-exp (first es) previous-env)]
        (recur (rest es) updatedEnv (conj results res))))))

(defn parse-atom [str]
  (cond 
    (= "true" str) true
    (= "false" str) false
    :else (try (Long/parseLong str) 
            (catch NumberFormatException e 
              (try (Double/parseDouble str)
                (catch NumberFormatException e
                  str))))))
    
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

(defn result-or-error [func env]
  (try (func) 
    (catch Exception e [(str "Error: " (.getMessage e)) env])))  

(defn prompt []
  (print "==> ")
  (flush))

(defn repl []
  (println "Welcome to tiny-lisp!")
  (prompt)
  (let [input-lines (line-seq (java.io.BufferedReader. *in*))]
    (loop [lines input-lines
           current-env default-env]
      (if (not (empty? lines))
        (let [[result new-env] (result-or-error #(eval-exp (first (parse (tokenize (first lines)))) current-env) current-env)]
          (println (expr->string result))
          (prompt)
          (recur (rest lines) new-env))))
    ))

(defn -main []
  (repl))
