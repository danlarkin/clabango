(ns clabango.test.parser
  (:use [clojure.test]
        [clabango.parser]))

(deftest lex-tests
  (is (= (lex "a b c d")
         [{:offset 1 :line 1 :file "UNKNOWN" :token "a b c d"}]))
  (is (= (lex "a {{ b c d")
         [{:offset 1 :line 1 :file "UNKNOWN" :token "a "}
          {:offset 3 :line 1 :file "UNKNOWN" :token :open-filter}
          {:offset 5 :line 1 :file "UNKNOWN" :token " b c d"}]))
  (is (= (lex "a {{ b c }}d")
         [{:offset 1 :line 1 :file "UNKNOWN" :token "a "}
          {:offset 3 :line 1 :file "UNKNOWN" :token :open-filter}
          {:offset 5 :line 1 :file "UNKNOWN" :token " b c "}
          {:offset 10 :line 1 :file "UNKNOWN" :token :close-filter}
          {:offset 12 :line 1 :file "UNKNOWN" :token "d"}]))
  (is (= (lex "a {{ b c d}")
         [{:offset 1 :line 1 :file "UNKNOWN" :token "a "}
          {:offset 3 :line 1 :file "UNKNOWN" :token :open-filter}
          {:offset 5 :line 1 :file "UNKNOWN" :token " b c d}"}]))
  (is (= (lex "a {{ b c d%}")
         [{:offset 1 :line 1 :file "UNKNOWN" :token "a "}
          {:offset 3 :line 1 :file "UNKNOWN" :token :open-filter}
          {:offset 5 :line 1 :file "UNKNOWN" :token " b c d"}
          {:offset 11 :line 1 :file "UNKNOWN" :token :close-tag}]))
  (is (= (lex "a {%foo%}")
         [{:offset 1 :line 1 :file "UNKNOWN" :token "a "}
          {:offset 3 :line 1 :file "UNKNOWN" :token :open-tag}
          {:offset 5 :line 1 :file "UNKNOWN" :token "foo"}
          {:offset 8 :line 1 :file "UNKNOWN" :token :close-tag}]))
  (is (= (lex "a {%foo%}\nline 2!")
         [{:offset 1 :line 1 :file "UNKNOWN" :token "a "}
          {:offset 3 :line 1 :file "UNKNOWN" :token :open-tag}
          {:offset 5 :line 1 :file "UNKNOWN" :token "foo"}
          {:offset 8 :line 1 :file "UNKNOWN" :token :close-tag}
          {:offset 10 :line 1 :file "UNKNOWN" :token "\n"}
          {:offset 1 :line 2 :file "UNKNOWN" :token "line 2!"}])))

(deftest passthrough
  (let [s "a b c d"]
    (is (= s (render s {}))))
  (let [s "{a b c } d"]
    (is (= s (render s {}))))
  (let [s "{a b c }\n d"]
    (is (= s (render s {})))))

(deftest filter-upper
  (is (= "FOO" (render "{{f|upper}}" {:f "foo"}))))
