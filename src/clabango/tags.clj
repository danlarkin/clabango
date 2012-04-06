(ns clabango.tags
  (:use [clojure.java.io :only [file]]))

(defn load-template [template]
  (-> (Thread/currentThread)
      (.getContextClassLoader)
      (.getResource template)
      file))

;; template-tag must return a 2-vector of [string context]
;; where string will be parsed and context will be the new
;; context passed along to the rest of the pipeline
;; (including parsing of string)

;; the reason template-tag needs to return context is some
;; tags need to keep track of state and the context is the
;; logical place to do that (rather than a ref or atom)

(defmulti template-tag (fn [tag-name & _] tag-name))

(defmethod template-tag "include" [_ nodes context]
  (let [[node] nodes
        [template] (:args node)
        template (second (re-find #"\"(.*)\"" template))]
    [(load-template template)
     context]))

(defmethod template-tag "block" [_ nodes context]
  (let [block-node (first nodes)
        block-name (first (:args block-node))]
    [(let [body-nodes (rest (butlast nodes))]
       (if (seq body-nodes)
         (for [node body-nodes]
           (assoc node
             :block-name block-name
             :block-metadata (select-keys (:body block-node)
                                          #{:offset :line :file})))
         [{:block-name block-name
           :block-metadata (select-keys (:body block-node)
                                        #{:offset :line :file})
           :type :noop}]))
     context]))

(defmethod template-tag "extends" [_ nodes context]
  (let [[s context] (template-tag "include" nodes context)]
    [s
     (assoc context :extended true)]))

(defmethod template-tag "if" [_ nodes context]
  (let [if-node (first nodes)
        decision (first (:args if-node))
        body-nodes (rest (butlast nodes))]
    [(if (context (keyword decision))
       body-nodes
       [{:body (dissoc (:body if-node) :token)
         :type :noop}])
     context]))

(defmethod template-tag "with-foo-as-42" [_ nodes context]
  [(rest (butlast nodes)) (assoc context :foo 42)])
