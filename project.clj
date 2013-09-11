(defproject tiny-lisp "0.1.0"
  :description "Simple Lisp interpreter written in Clojure"
  :url "http://www.janvsmachine.net"
  :license {:name "Public Domain/Unlicense"
            :url "http://unlicense.org"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [net.mikera/cljunit "0.2.0"]
                 [org.clojure/core.match "0.2.0-rc5"]]
  :main tiny-lisp.core)
