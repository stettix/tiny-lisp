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

(defn tokenize [str]
  (filter (fn [s] (> (count s) 0)) 
    (string/split (replace str #"([\(\)])" " $0 ") #"\s")))

(defn parse [str]
  (try-or 
    (Long/parseLong str) 
    (Double/parseDouble str) 
    (Boolean/parseBoolean str) 
    str))

