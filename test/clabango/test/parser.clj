(ns clabango.test.parser
  (:use [clojure.test]
        [clabango.parser]))

(deftest passthrough
  (let [s "a b c d"]
    (is (= s (render s {}))))
  (let [s "{a b c } d"]
    (is (= s (render s {})))))

(deftest filter-upper
  (is (= "FOO" (render "{{f|upper}}" {"f" "foo"}))))
