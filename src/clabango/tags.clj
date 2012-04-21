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

(def valid-tags (atom {}))

(defn fix-args
  [fn-tail]
  (let [[f & r] fn-tail]
    `(~(vec (cons '_ f))
      ~@r)))

(defmacro deftemplatetag [open-tag & args]
  (let [[close-tag fn-tail] (if (string? (first args))
                              [(first args) (rest args)]
                              [:inline args])]
    (swap! valid-tags assoc open-tag close-tag)
    `(defmethod template-tag ~open-tag ~@(fix-args fn-tail))))

(defmulti template-tag (fn [tag-name & _] tag-name))

(deftemplatetag "include" [nodes context]
  (let [[node] nodes
        [template] (:args node)
        template (second (re-find #"\"(.*)\"" template))]
    {:string (load-template template)}))

(deftemplatetag "block" "endblock" [nodes context]
  (let [block-node (first nodes)
        block-name (first (:args block-node))]
    (let [body-nodes (rest (butlast nodes))]
      {:nodes (if (seq body-nodes)
                (for [node body-nodes]
                  (assoc node
                    :block-name block-name
                    :block-metadata (select-keys (:body block-node)
                                                 #{:offset :line :file})))
                [{:block-name block-name
                  :block-metadata (select-keys (:body block-node)
                                               #{:offset :line :file})
                  :type :noop}])})))

(deftemplatetag "extends" [nodes context]
  (let [{:keys [string]} (template-tag "include" nodes context)]
    {:string string
     :context (assoc context :extended true)}))

(deftemplatetag "if" "endif" [nodes context]
  (let [if-node (first nodes)
        decision (first (:args if-node))
        body-nodes (rest (butlast nodes))]
    {:nodes (if (context (keyword decision))
              body-nodes
              [{:body (dissoc (:body if-node) :token)
                :type :noop}])}))

(deftemplatetag "ifequal" "endifequal" [nodes context]
  (let [if-node (first nodes)
        operands (:args if-node)
        body-nodes (rest (butlast nodes))]
    {:nodes (if (apply = (for [op operands]
                           (if (= \" (.charAt op 0))
                             (subs op 1 (dec (count op)))
                             (context (keyword op)))))
              body-nodes
              [{:body (dissoc (:body if-node) :token)
                :type :noop}])}))
