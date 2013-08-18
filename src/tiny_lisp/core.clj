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

; Represent environment as maps that map symbol names to values.
(def empty-env {})

; A basic set of operations.
(def default-env 
  { "+" +, "-" -, "*" *, "/" /, "<" <, "<=" <=, ">" >, ">=" >=, "=" = })

; Return an environment that includes the new value.
(defn env-set [env symbol value] 
    (assoc env symbol value))

; Return an environment where the 'inner' environment takes precedence, then the 'outer'.
(defn wrap-env [inner-env outer-env] 
  (merge outer-env inner-env))

(defn error [message]
  (throw (IllegalArgumentException. message)))

(defn error-if [has-error message]
  (if has-error (error message)))

(declare eval-exprs)

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
                              _ (error-if arg3 "Exactly two arguments excpected for 'cdr'")
                              [[res1 res2] env] (eval-exprs [arg1 arg2] env)]
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
                          (if test-result 
                            (eval-exp conseq newEnv) 
                            (eval-exp alt newEnv)))
    
    [["lambda" & args]] (let [[arg-names lambda-expr rest] args
                              _ (error-if (or (nil? arg-names) (nil? lambda-expr) rest) "Expected two arguments for 'lambda'")
                              lambda (fn [& arg-values]
                                       (let [args-env (zipmap arg-names arg-values)
                                             new-env (wrap-env args-env env)]
                                         (let [[lambda-result lambda-result-env] (eval-exp lambda-expr new-env)]
                                           lambda-result)
                                         ))]
                          [lambda env])
    
    :else (if (coll? expr)
            ;; Evaluate procedure call.
            (let [[evalled-args _](eval-exprs expr env)
                  proc (first evalled-args)
                  proc-args (rest evalled-args)
                  result (apply proc proc-args)]
              [result env])
            ;; Simple value evaluates as itself.
            [expr env])
    ))

; Evaluates all given expressions, chaining the environment returned by each evaluation
; through to the next one. Returns [list of results of the evaluation, final environment]
; It's basically a map - except we have to pass the updated environments along. 
; Can we do this more neatly?
(defn eval-exprs [exprs env]
  (loop [es exprs
         previous-env env
         results []]
    (if (empty? es)
      [results previous-env]
      (let [[res updatedEnv] (eval-exp (first es) previous-env)]
        (recur (rest es) updatedEnv (conj results res)))))
  )

; TODO: have a block for all cases that take args that need to be evaluated, and evaluate them 
; all together before proceeding? Then again, I want to do error checking first... and that depends on how
; many args each form takes.

; TODO: Change error-if to take varargs of booleans? Or doesn't that go well with the msg? Maybe a list then?
; Or better: overload with taking a single vs. a sequence as arguments?

; TODO: Reconsider the use of the try-or macro here, as there are only two cases now. Wrap in a function instead?
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

