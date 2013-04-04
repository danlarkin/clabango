(defproject clabango "0.6-SNAPSHOT"
  :description "A templating language?"
  :url "https://github.com/danlarkin/clabango/"
  :license {:name "3-Clause BSD"
            :url "http://opensource.org/licenses/BSD-3-Clause"
            :distribution :repo}
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [cheshire "5.0.2"]
                 [commons-codec "1.6"]
                 [joda-time "2.1"]
                 [net.sf.opencsv/opencsv "2.3"]
                 [criterium "0.3.1" :scope "test"]
                 [org.apache.commons/commons-lang3 "3.1"]]
  :test-selectors {:default (complement :benchmark)
                   :benchmark :benchmark
                   :all (constantly true)})
