(ns clabango.test.parser
  (:use [clojure.test]
        [clabango.parser]))

(deftest lex-tests
  (is (= (lex "a b c d")
         [{:started 1 :token "a b c d"}]))
  (is (= (lex "a {{ b c d")
         [{:started 1 :token "a "}
          {:started 3 :token :open-filter}
          {:started 5 :token " b c d"}]))
  (is (= (lex "a {{ b c }}d")
         [{:started 1 :token "a "}
          {:started 3 :token :open-filter}
          {:started 5 :token " b c "}
          {:started 10 :token :close-filter}
          {:started 12 :token "d"}]))
  (is (= (lex "a {{ b c d}")
         [{:started 1 :token "a "}
          {:started 3 :token :open-filter}
          {:started 5 :token " b c d}"}]))
  (is (= (lex "a {{ b c d%}")
         [{:started 1 :token "a "}
          {:started 3 :token :open-filter}
          {:started 5 :token " b c d"}
          {:started 11 :token :close-tag}]))
  (is (= (lex "a {%foo%}")
         [{:started 1 :token "a "}
          {:started 3 :token :open-tag}
          {:started 5 :token "foo"}
          {:started 8 :token :close-tag}])))

(deftest passthrough
  (let [s "a b c d"]
    (is (= s (render s {}))))
  (let [s "{a b c } d"]
    (is (= s (render s {})))))

(deftest filter-upper
  (is (= "FOO" (render "{{f|upper}}" {"f" "foo"}))))
