(ns clabango.test.parser
  (:require [clojure.test :refer :all]
            [clabango.parser :refer :all]
            [clabango.tags :refer :all])
  (:import (java.util Date)))

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

(deftest test-include
  (let [s (str "{{foo}}\n dogs live in the park."
               " {% include \"clabango/templates/foo.html\" %}")]
    (is (= (parse s {:foo 42 :name "Dan"})
           [{:type :string
             :safe? false
             :body {:token "42"
                    :offset 3
                    :line 1
                    :file "UNKNOWN"}}
            {:type :string
             :safe? true
             :body {:token "\n"
                    :offset 8
                    :line 1
                    :file "UNKNOWN"}}
            {:type :string
             :safe? true
             :body {:token " dogs live in the park. "
                    :offset 1
                    :line 2
                    :file "UNKNOWN"}}
            {:type :string
             :safe? true
             :body {:token "Hello, "
                    :offset 1
                    :line 1
                    :file (load-template "clabango/templates/foo.html")}}
            {:type :string
             :safe? false
             :body {:token "Dan"
                    :offset 10
                    :line 1
                    :file (load-template "clabango/templates/foo.html")}}
            {:type :string
             :safe? true
             :body {:token "!"
                    :offset 16
                    :line 1
                    :file (load-template "clabango/templates/foo.html")}}
            {:type :string
             :safe? true
             :body {:token "\n"
                    :offset 17
                    :line 1
                    :file (load-template "clabango/templates/foo.html")}}]))
    (is (= (render s {:foo 42 :name "Dan"})
           "42\n dogs live in the park. Hello, Dan!\n"))))

(deftest test-block-overriding
  (is (= (render (str "{% block foo%}default {{bar}}{%endblock%}"
                      "1234{%block foo%}override {{bar}} {%endblock%}")
                 {:bar "BAR"})
         "override BAR 1234"))
  (testing "empty overrides should work"
    (is (= (render (str "{% block foo%}default {{bar}}{%endblock%}"
                        "{% block bar%}default bar{%endblock%}"
                        "{%block foo%}{%endblock%}{%block bar%}{%endblock%}")
                   {:bar "BAR"})
           "")))
  (testing "works when extending"
    (testing "override first block"
      (is (= (render (str "{% extends \"clabango/templates/blocks.html\" %}"
                          "{% block foo %}overriding foo now{% endblock %}")
                     {:foo 12
                      :bar 47})
             (str "overriding foo now\n"
                  "Here's the default text of bar 47\n"))))
    (testing "override second block"
      (is (= (render (str "{% extends \"clabango/templates/blocks.html\" %}"
                          "{% block bar %}override {{foo}}{% endblock %}")
                     {:foo 12
                      :bar 47})
             (str "Here's the default text of foo 12\n"
                  "override 12\n"))))
    (testing "override both blocks"
      (is (= (render (str "{% extends \"clabango/templates/blocks.html\" %}"
                          "{% block foo %}new foo!{% endblock %}"
                          "{% block bar %}new bar {{foo}} {{bar}}{% endblock %}")
                     {:foo 12
                      :bar 47})
             (str "new foo!\n"
                  "new bar 12 47\n"))))))

(deftest test-nested-blocks
  (is (= (render (str "{% block foo %}"
                      "  foo1"
                      "  {% block bar %}"
                      "    bar1"
                      "  {% endblock %}"
                      "  foo2"
                      "{% endblock %}")
                 {})
         "  foo1      bar1    foo2")))

(deftest extends-plus-include-inside-a-block
  (is (= (render (str "{% extends \"clabango/templates/blocks.html\" %}"
                      "{%block foo%}"
                      "{% include \"clabango/templates/foo.html\" %}"
                      "{% endblock %}")
                 {:bar "BAR" :name "Dan"})
         "Hello, Dan!\n\nHere's the default text of bar BAR\n")))

(deftest overriding-blocks-inside-other-blocks
  (is (= (render
          (str "{% block a %}first a{% endblock%}"
               "{%block a%}"
               "second a start"
               "{%block b %}{% endblock%}"
               "second a stop"
               "{% endblock %}"
               "{%block b%}second b{%endblock%}")
          {})
         (str "second a start"
              "second b"
              "second a stop"))))

(deftest overriding-blocks-inside-other-blocks-through-template-inheritance
  (is (= (render-file "clabango/templates/inherit-c.html" {})
         (str "start a\n"
              "\n"
              "start b\n"
              "\n"
              "start c\n"
              "stop c\n"
              "\n"
              "stop b\n"
              "\n"
              "stop a\n"
              "\n"
              "\n"
              "\n"
              "\n"
              "\n"))))

(deftest test-if
  (is (= (render "{% if foo %}foo is true{% endif %}" {:foo true})
         "foo is true"))
  (is (= (render "{% if foo %}foo is true{% endif %}" {:foo false})
         ""))
  (is (= (render "{% if foo %}foo is true{% else %}foo is false{% endif %}"
                 {:foo true})
         "foo is true"))
  (is (= (render "{% if foo %}foo is true{% else %}foo is false{% endif %}"
                 {:foo false})
         "foo is false"))
  (let [template
        "{% if foo %}
         foo is true
         {% if bar %}bar is also true{% endif %}
         {% else %} foo is false
         {% if baz %}but baz is true {% else %}baz is also false{% endif %}
         {% endif %}"]
    (is (= (render template {:foo true :bar true :baz false})
           "\n         foo is true\n         bar is also true\n         "))
    (is (= (render template {:foo false :bar true :baz false})
           " foo is false\n         baz is also false\n         "))
    (is (= (render template {:foo false :bar true :baz true})
           " foo is false\n         but baz is true \n         ")))
  (is (thrown? Exception (render "foo {% else %} bar" {}))))

(deftest test-if-not
  (is (= (render "{% if not foo %}foo is true{% endif %}" {:foo true})
         ""))
  (is (= (render "{% if not foo %}foo is true{% endif %}" {:foo false})
         "foo is true")))

(deftest test-nested-if
  (is (= (render (str "{% if foo %}before bar {% if bar %}"
                      "foo & bar are true"
                      "{% endif %} after bar{% endif %}")
                 {:foo true
                  :bar true})
         "before bar foo & bar are true after bar")))

(deftest test-ifequal
  (is (= (render "{% ifequal foo \"foo\" %}yez{% endifequal %}" {:foo "foo"})
         "yez"))
  (is (= (render "{% ifequal foo \"foo\" bar %}yez{% endifequal %}"
                 {:foo "foo"
                  :bar "foo"})
         "yez"))
  (is (= (render "{% ifequal foo \"foo\" bar %}yez{% endifequal %}"
                 {:foo "foo"
                  :bar "bar"})
         ""))
  (is (= (render "{% ifequal foo \"foo\" %}foo{% else %}no foo{% endifequal %}"
                 {:foo "foo"})
         "foo"))
  (is (= (render "{% ifequal foo \"foo\" %}foo{% else %}no foo{% endifequal %}"
                 {:foo false})
         "no foo")))

(deftest test-for
  (is (= (render "{% for ele in foo %}<<{{ele}}>>{%endfor%}"
                 {:foo [1 2 3]})
         "<<1>><<2>><<3>>")))

(deftest test-map-lookup
  (is (= (render "{{foo}}" {:foo {:bar 42}})
         "{:bar 42}"))
  (is (= (render "{{foo.bar}}" {:foo {:bar 42}})
         "42")))

(deftest nested-forloop-first
  (is (= (render (str "{% for x in list1 %}"
                      "{% for y in list2 %}"
                      "{{x}}-{{y}}"
                      "{% if forloop.first %}'{% endif %} "
                      "{% endfor %}{% endfor %}")
                 {:list1 '[a b c]
                  :list2 '[1 2 3]})
         "a-1' a-2 a-3 b-1' b-2 b-3 c-1' c-2 c-3 ")))

(deftest forloop-first-last
  (let [t (str "{% for x in f %}"
               "{{x}}.{{forloop.first}}.{{forloop.last}},"
               "{% endfor %}")]
    (is (= "" (render t {:f []})))
    (is (= "1.true.true," (render t {:f [1]})))
    (is (= "1.true.false,2.false.true," (render t {:f [1 2]})))
    (is (= "1.true.false,2.false.false,3.false.true,"
           (render t {:f [1 2 3]})))))

(deftest forloop-with-one-element
  (is (= (render (str "{% for x in list %}"
                      "-{{x}}"
                      "{% endfor %}")
                 {:list '[a]})
         "-a")))

(deftest forloop-with-no-elements
  (is (= (render (str "before{% for x in list %}"
                      "-{{x}}"
                      "{% endfor %}after")
                 {:list '[]})
         "beforeafter")))

(deftest filter-upper
  (is (= "FOO" (render "{{f|upper}}" {:f "foo"}))))

(deftest filter-no-value
  (is (= "" (render "{{f|upper}}" {}))))

(deftest filter-date
  (is (= "1970-01-01_00:00:00"
         (render "{{f|date:\"yyyy-MM-dd_HH:mm:ss\"}}" {:f (Date. (long 0))}))))

(deftest filter-hash-md5
  (is (= "acbd18db4cc2f85cedef654fccc4a4d8"
         (render "{{f|hash:\"md5\"}}" {:f "foo"}))))

(deftest filter-hash-sha512
  (is (= (str "f7fbba6e0636f890e56fbbf3283e524c6fa3204ae298382d624741d"
              "0dc6638326e282c41be5e4254d8820772c5518a2c5a8c0c7f7eda19"
              "594a7eb539453e1ed7")
         (render "{{f|hash:\"sha512\"}}" {:f "foo"}))))

(deftest filter-hash-invalid-hash
  (is (thrown? Exception (render "{{f|hash:\"foo\"}}" {:f "foo"}))))

(deftest filter-hash-no-hash-given
  (is (thrown? Exception (render "{{f|hash}}" {:f "foo"}))))

(deftest filter-count
  (is (= "3" (render "{{f|count}}" {:f "foo"})))
  (is (= "4" (render "{{f|count}}" {:f [1 2 3 4]})))
  (is (= "0" (render "{{f|count}}" {:f []})))
  (is (= "0" (render "{{f|count}}" {}))))

(deftest filter-pluralize
  (is (= "s" (render "{{f|pluralize}}" {:f []})))
  (is (= "" (render "{{f|pluralize}}" {:f [1]})))
  (is (= "s" (render "{{f|pluralize}}" {:f [1 2 3]})))

  (is (= "ies" (render "{{f|pluralize:\"ies\"}}" {:f []})))
  (is (= "" (render "{{f|pluralize:\"ies\"}}" {:f [1]})))
  (is (= "ies" (render "{{f|pluralize:\"ies\"}}" {:f [1 2 3]})))

  (is (= "ies" (render "{{f|pluralize:\"y,ies\"}}" {:f []})))
  (is (= "y" (render "{{f|pluralize:\"y,ies\"}}" {:f [1]})))
  (is (= "ies" (render "{{f|pluralize:\"y,ies\"}}" {:f [1 2 3]}))))

(deftest filter-to-json
  (is (= "1" (render "{{f|to-json}}" {:f 1})))
  (is (= "[1]" (render "{{f|to-json}}" {:f [1]})))
  (is (= "{&quot;foo&quot;:27,&quot;dan&quot;:&quot;awesome&quot;}"
         (render "{{f|to-json}}" {:f {:foo 27 :dan "awesome"}})))
  (is (= "{\"foo\":27,\"dan\":\"awesome\"}"
         (render "{{f|to-json|safe}}" {:f {:foo 27 :dan "awesome"}})))
  (is (= "{\"foo\":27,\"dan\":\"awesome\"}"
         (render "{{f|safe|to-json}}" {:f {:foo 27 :dan "awesome"}})))
  (is (= "null" (render "{{f|to-json}}" {}))))

(deftest filter-chaining
  (is (= "ACBD18DB4CC2F85CEDEF654FCCC4A4D8"
         (render "{{f|hash:\"md5\"|upper}}" {:f "foo"}))))

(deftest test-escaping
  (is (= "<tag>&lt;foo bar=&quot;baz&quot;&gt;\\&gt;</tag>"
         (render "<tag>{{f}}</tag>" {:f "<foo bar=\"baz\">\\>"})))
  (is (= "&amp;&trade;&eacute;"
         (render "{{f}}" {:f "&™é"}))))

(deftest test-safe-filter
  (is (= "&lt;foo&gt;"
         (render "{{f}}" {:f "<foo>"})))
  (is (= "<foo>"
         (render "{{f|safe}}" {:f "<foo>"})))
  (is (= "<FOO>"
         (render "{{f|upper|safe}}" {:f "<foo>"})))
  (is (= "<FOO>"
         (render "{{f|safe|upper}}" {:f "<foo>"})))
  (is (= "<FOO>"
         (render "{{f|safe|upper|safe}}" {:f "<foo>"}))))
