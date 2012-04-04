(ns clabango.test.parser
  (:use [clojure.test]
        [clabango.parser]))

(deftest lex-tests
  (is (= (lex "a b c d")
         [{:started 1 :file "UNKNOWN" :token "a b c d"}]))
  (is (= (lex "a {{ b c d")
         [{:started 1 :file "UNKNOWN" :token "a "}
          {:started 3 :file "UNKNOWN" :token :open-filter}
          {:started 5 :file "UNKNOWN" :token " b c d"}]))
  (is (= (lex "a {{ b c }}d")
         [{:started 1 :file "UNKNOWN" :token "a "}
          {:started 3 :file "UNKNOWN" :token :open-filter}
          {:started 5 :file "UNKNOWN" :token " b c "}
          {:started 10 :file "UNKNOWN" :token :close-filter}
          {:started 12 :file "UNKNOWN" :token "d"}]))
  (is (= (lex "a {{ b c d}")
         [{:started 1 :file "UNKNOWN" :token "a "}
          {:started 3 :file "UNKNOWN" :token :open-filter}
          {:started 5 :file "UNKNOWN" :token " b c d}"}]))
  (is (= (lex "a {{ b c d%}")
         [{:started 1 :file "UNKNOWN" :token "a "}
          {:started 3 :file "UNKNOWN" :token :open-filter}
          {:started 5 :file "UNKNOWN" :token " b c d"}
          {:started 11 :file "UNKNOWN" :token :close-tag}]))
  (is (= (lex "a {%foo%}")
         [{:started 1 :file "UNKNOWN" :token "a "}
          {:started 3 :file "UNKNOWN" :token :open-tag}
          {:started 5 :file "UNKNOWN" :token "foo"}
          {:started 8 :file "UNKNOWN" :token :close-tag}])))

(deftest passthrough
  (let [s "a b c d"]
    (is (= s (render s {}))))
  (let [s "{a b c } d"]
    (is (= s (render s {})))))

(deftest filter-upper
  (is (= "FOO" (render "{{f|upper}}" {:f "foo"}))))

#_(deftest probably-fails
    (render "{{{what goes here" {})
    (render "{{{{what goes here" {})
    (render "{{{what goes here}}" {})
    )
