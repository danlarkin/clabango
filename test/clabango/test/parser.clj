(ns clabango.test.parser
  (:use [clojure.test]
        [clabango.parser]))

(deftest lex-tests
  (is (= (lex "a b c d")
         ["a b c d"]))
  (is (= (lex "a {{ b c d")
         ["a " :open-filter " b c d"]))
  (is (= (lex "a {{ b c }}d")
         ["a " :open-filter " b c " :close-filter "d"]))
  (is (= (lex "a {{ b c d}")
         ["a " :open-filter " b c d}"]))
  (is (= (lex "a {{ b c d%}")
         ["a " :open-filter " b c d" :close-tag]))
  (is (= (lex "a {%foo%}")
         ["a " :open-tag "foo" :close-tag])))

(deftest passthrough
  (let [s "a b c d"]
    (is (= s (render s {}))))
  (let [s "{a b c } d"]
    (is (= s (render s {})))))

(deftest filter-upper
  (is (= "FOO" (render "{{f|upper}}" {"f" "foo"}))))
