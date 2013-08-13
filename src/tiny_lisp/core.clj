(ns tiny-lisp.core
  (require [clojure.string :as string])
  (use [clojure.core.match :only (match)]))

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
    [(s :guard string?)] (get env expr)
    [[ (:or "q" "quote") & args ]] args
    [[ "atom" & args ]] (not (seq? (eval-exp args env)))
    :else expr)
)

;(defn eval-exp [expr env]
;  (let [op (first expr)
;        args (rest expr)]
;    (println op ":" args)
;    (cond
;      (string? expr) (get env expr)
;      (= "q" op) (eval-exp args env)
;      (= "atom?" op) (not (seq? (eval-exp args env)))
;      ; TODO - a few more details needed here...
;      :else [expr env])))

(defn parse-atom [str](eval-exp '("atom?" 42) {})
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
