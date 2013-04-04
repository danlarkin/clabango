(ns clabango.test.benchmark
  (:require [clojure.test :refer :all]
            [clabango.parser :refer [render-file]]
            [criterium.core :as criterium]))

(defn make-things [n]
  (repeat n {:foo "Lorem ipsum dolor sit amet, consectetur
                   adipisicing elit, sed do eiusmod tempor incididunt ut
                   labore et dolore magna aliqua. Ut enim ad minim veniam,
                   quis nostrud exercitation ullamco laboris nisi ut aliquip
                   ex ea commodo consequat. Duis aute irure dolor in
                   reprehenderit in voluptate velit esse cillum dolore eu
                   fugiat nulla pariatur. Excepteur sint occaecat cupidatat
                   non proident, sunt in culpa qui officia deserunt mollit
                   anim id est laborum."
             :bar 42}))

(deftest ^:benchmark bench-site []
  (let [foos (doall (repeatedly 5 (partial repeat 10 "foo")))
        things (doall (make-things 100))]
    (criterium/bench
     (render-file "clabango/templates/benchmark/main"
                  {:message "this is the message"
                   :foos foos
                   :things things}))))
