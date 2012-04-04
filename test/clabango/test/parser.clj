(ns clabango.test.parser
  (:use [clojure.test]
        [clabango.parser]))

(deftest lex-tests
  (is (= (lex "a b c d")
         [{:started 1 :line 1 :file "UNKNOWN" :token "a b c d"}]))
  (is (= (lex "a {{ b c d")
         [{:started 1 :line 1 :file "UNKNOWN" :token "a "}
          {:started 3 :line 1 :file "UNKNOWN" :token :open-filter}
          {:started 5 :line 1 :file "UNKNOWN" :token " b c d"}]))
  (is (= (lex "a {{ b c }}d")
         [{:started 1 :line 1 :file "UNKNOWN" :token "a "}
          {:started 3 :line 1 :file "UNKNOWN" :token :open-filter}
          {:started 5 :line 1 :file "UNKNOWN" :token " b c "}
          {:started 10 :line 1 :file "UNKNOWN" :token :close-filter}
          {:started 12 :line 1 :file "UNKNOWN" :token "d"}]))
  (is (= (lex "a {{ b c d}")
         [{:started 1 :line 1 :file "UNKNOWN" :token "a "}
          {:started 3 :line 1 :file "UNKNOWN" :token :open-filter}
          {:started 5 :line 1 :file "UNKNOWN" :token " b c d}"}]))
  (is (= (lex "a {{ b c d%}")
         [{:started 1 :line 1 :file "UNKNOWN" :token "a "}
          {:started 3 :line 1 :file "UNKNOWN" :token :open-filter}
          {:started 5 :line 1 :file "UNKNOWN" :token " b c d"}
          {:started 11 :line 1 :file "UNKNOWN" :token :close-tag}]))
  (is (= (lex "a {%foo%}")
         [{:started 1 :line 1 :file "UNKNOWN" :token "a "}
          {:started 3 :line 1 :file "UNKNOWN" :token :open-tag}
          {:started 5 :line 1 :file "UNKNOWN" :token "foo"}
          {:started 8 :line 1 :file "UNKNOWN" :token :close-tag}]))
  (is (= (lex "a {%foo%}\nline 2!")
         [{:started 1 :line 1 :file "UNKNOWN" :token "a "}
          {:started 3 :line 1 :file "UNKNOWN" :token :open-tag}
          {:started 5 :line 1 :file "UNKNOWN" :token "foo\n"}
          {:started 8 :line 1 :file "UNKNOWN" :token :close-tag}
          {:started 1 :line 2 :file "UNKNOWN" :token "line 2!"}])))

(deftest passthrough
  (let [s "a b c d"]
    (is (= s (render s {}))))
  (let [s "{a b c } d"]
    (is (= s (render s {})))))

(deftest filter-upper
  (is (= "FOO" (render "{{f|upper}}" {:f "foo"}))))
