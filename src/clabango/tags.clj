(ns clabango.tags
  (:use [clojure.java.io :only [file]]
        [clabango.filters :only [context-lookup]]))

(defn load-template [template]
  (-> (Thread/currentThread)
      (.getContextClassLoader)
      (.getResource template)
      file))

(defn get-block-status [context]
  (::block-info context))

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
    {:string (load-template template)
     :context context}))

(deftemplatetag "block" "endblock" [nodes context]
  (let [block-node (first nodes)
        block-name (first (:args block-node))
        block-info {:block-name block-name
                    :block-metadata (select-keys (:body block-node)
                                                 #{:offset :line :file})}]
    (let [body-nodes (rest (butlast nodes))]
      {:nodes (if (seq body-nodes)
                (for [node body-nodes]
                  (merge node block-info))
                [(assoc block-info :type :noop)])
       :context (assoc context ::block-info block-info)})))

(deftemplatetag "extends" [nodes context]
  (let [{:keys [string]} (template-tag "include" nodes context)]
    {:string string
     :context (assoc context :extended true)}))

(deftemplatetag "if" "endif" [nodes context]
  (let [if-node (first nodes)
        args (:args if-node)
        body-nodes (rest (butlast nodes))
        [flip decision] (cond (= 1 (count args))
                              [identity (first args)]

                              (and (= 2 (count args))
                                   (= "not" (first args)))
                              [not (second args)]

                              :default (throw (Exception. (str "Syntax error: "
                                                               if-node))))]
    {:nodes (if (flip (context-lookup context decision))
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
                             (context-lookup context op))))
              body-nodes
              [{:body (dissoc (:body if-node) :token)
                :type :noop}])}))

(deftemplatetag "for" "endfor" [nodes context]
  (let [for-node (first nodes)
        [x in coll] (:args for-node)
        body-nodes (rest (butlast nodes))]
    (if (= in "in")
      (let [coll (context-lookup context coll)]
        {:groups (cons {:nodes body-nodes
                        :context (assoc context
                                   (keyword x) (first coll)
                                   :forloop {:first true})}
                       (for [ele (rest coll)]
                         {:nodes body-nodes
                          :context (assoc context (keyword x) ele)}))})
      (throw (Exception. (str "syntax error in:" for-node))))))
