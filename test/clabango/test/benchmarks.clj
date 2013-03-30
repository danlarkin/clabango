(ns clabango.test.benchmarks
  (:require [clojure.test :refer :all]
            [criterium.core :refer [quick-bench]]          
            [clabango.test.parser :refer :all]))

(defmacro run-benchmark [benchmark]
  `(do 
     (println "\nbenchmarking:" ~(name benchmark))
     (quick-bench (~benchmark))))

(deftest ^{:benchmarks true} benchmarks
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
