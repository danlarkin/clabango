(ns clabango.test.benchmarks
  (:require [clojure.test :refer :all]
            [criterium.core :refer [quick-bench]]          
            [clabango.test.parser :refer :all]
            [clabango.parser :refer [render-file]]))

(defmacro run-benchmark [benchmark]
  `(do 
     (println "\nbenchmarking:" ~(name benchmark))
     (quick-bench (~benchmark))))

(deftest ^{:element-benchmarks true} benchmarks
  (run-benchmark test-include)
  (run-benchmark passthrough)  
  (run-benchmark test-block-overriding)
  (run-benchmark test-nested-blocks)
  (run-benchmark extends-plus-include-inside-a-block)
  (run-benchmark overriding-blocks-inside-other-blocks)
  (run-benchmark overriding-blocks-inside-other-blocks-through-template-inheritance)
  (run-benchmark test-if)
  (run-benchmark test-if-not)
  (run-benchmark test-nested-if)
  (run-benchmark test-ifequal)
  (run-benchmark test-for)
  (run-benchmark test-map-lookup)
  (run-benchmark nested-forloop-first)   
  (run-benchmark filter-upper)
  (run-benchmark filter-date)
  (run-benchmark filter-hash-md5)
  (run-benchmark filter-count)
  (run-benchmark filter-pluralize)
  (run-benchmark filter-chaining)
  (run-benchmark test-escaping)
  (run-benchmark test-safe-filter))

(defn gen-messages [n]
  (repeat n {:name "foo"
             :message "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
             :timestamp (java.util.Date.)}))

(deftest ^{:site-benchmarks true} bench-site []
  (println "\nbenchmarking:" "site")
  (let [messages (gen-messages 100)]
    (quick-bench
      (render-file "clabango/templates/benchmarks/bench-home.html"
                 {:name "foo"
                  :message "test message"
                  :error "oops!"
                  :messages messages}))))
