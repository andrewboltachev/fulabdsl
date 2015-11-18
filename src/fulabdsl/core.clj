(ns fulabdsl.core
  (:use
   [clojure.test :refer [deftest run-tests is]]
   )
  #_(:require
    )
  )

(require 
    '[regexpforobj.core] :reload
  )

(defn main
  "I don't do a whole lot."
  []
  (println "Hello, Peter!" regexpforobj.core/the-var)
  (run-tests 'fulabdsl.core)
  )

(deftest test1
  (is (= (+ 2 3) 5)))
