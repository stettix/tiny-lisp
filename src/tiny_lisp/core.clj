(ns tiny-lisp.core
  (require [clojure.string :as string]))
;; TODO: use 'require' instead of 'use' here!

(defmacro try-or
  "See http://clj-me.cgrand.net/2009/01/08/try-or-or-try-try-else-or-else-try/" 
  ([] nil)
  ([form] form)
  ([form & forms]
    `(try 
       ~form
       (catch Exception e#
         (try-or ~@forms)))))

(defn eval-expr [expr]
  ; TODO - a few more details needed here.
  expr)
  ;(.toString (first expr)))

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
  (println usedTokenCount ":" tokens " - " aggr)
  (cond
    (empty? tokens) (throw (IllegalArgumentException. "Missing ')'"))
    (= ")" (first tokens)) [(reverse aggr) (inc usedTokenCount)]
    (= "(" (first tokens)) (do
                             (let [[nestedList, consumedTokens]
                               (parse-list (rest tokens) 0 '())]
                               (recur 
                                 (drop consumedTokens tokens) 
                                 (+ usedTokenCount consumedTokens) 
                                 (cons aggr nestedList))))
    :else (recur (rest tokens) (inc usedTokenCount) (cons (parse-atom (first tokens)) aggr))
    ))

; list of tokens -> list of parsed expressions.
(defn parse [tokens]
  (get (parse-list (concat tokens '(")")) 0 '()) 0))

(defn tokenize [str]
  (remove #(string/blank? %) 
          (string/split (string/replace str #"([\(\)])" " $0 ") #"\s")))

(defn expr->string [expr] 
  (if (seq? expr) 
    (str "(" (string/join " " expr) ")")
    (.toString expr)))

(defn repl []
  (println "Welcome to tiny-lisp!")
  (doseq [line (line-seq (java.io.BufferedReader. *in*))]
    (println (expr->string (eval-expr (parse (tokenize line)))))
    ))

;(defn -main []
;  (repl)
;)
