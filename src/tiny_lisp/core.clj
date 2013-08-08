(ns tiny-lisp.core
  (use [clojure.string :as string]))

(defmacro try-or
  "See http://clj-me.cgrand.net/2009/01/08/try-or-or-try-try-else-or-else-try/" 
  ([] nil)
  ([form] form)
  ([form & forms]
    `(try 
       ~form
       (catch Exception e#
         (try-or ~@forms)))))

(defn eval [expr]
  ; TODO - a few more details needed here.
  (.toString expr))

(defn parse [tokens]
  tokens) ; // TODO: that's not quite right is it...

(defn parse-atom [str]
  (cond 
    (= "true" str) true
    (= "false" str) false
    :else (try-or 
            (Long/parseLong str) 
            (Double/parseDouble str) 
            str)))

(defn tokenize [str]
  (remove #(blank? %) 
          (string/split (replace str #"([\(\)])" " $0 ") #"\s")))

(defn print-result [v]
  (println v)) ; TODO: Render valid Lisp string.

(defn repl []
  (doseq [line (line-seq (java.io.BufferedReader. *in*))] 
    (print-result (eval (tokenize (parse line))))
    )
  )
