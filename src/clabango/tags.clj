(ns clabango.tags
  (:require [clabango.filters :refer [context-lookup]]))

(defn load-template [template]
  (-> (Thread/currentThread)
      (.getContextClassLoader)
      (.getResource template)))

(defn get-block-status [context]
  (::block-info context))

(defonce valid-tags (atom {}))

(defn fix-args
  [fn-tail]
  (let [[f & r] fn-tail]
    `(~(vec (cons '_ f))
      ~@r)))

(defmacro deftemplatetag [open-tag & args]
  (let [[close-tag fn-tail] (if (string? (first args))
                              [(first args) (rest args)]
                              [:inline args])]
    `(do
       (swap! valid-tags assoc ~open-tag ~close-tag)
       (defmethod template-tag ~open-tag ~@(fix-args fn-tail)))))

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
        body-nodes (rest (butlast nodes))]
    {:nodes [(merge {:type :block
                     :name block-name
                     :nodes body-nodes}
                    (select-keys (:body block-node)
                                 #{:offset :line :file}))]
     :context context}))

(deftemplatetag "extends" [nodes context]
  (template-tag "include" nodes context))

(deftemplatetag "else" [[node] context]
  (throw (Exception. (str "else tag is only allowed inside"
                          " if and ifequal tags: " node))))

(deftemplatetag "if" "endif" [[if-node & nodes] context]
  (let [args            (:args if-node)
        body-nodes      (butlast nodes)
        [flip decision] (cond (= 1 (count args))
                              [identity (first args)]

                              (and (= 2 (count args))
                                   (= "not" (first args)))
                              [not (second args)]

                              :default (throw (Exception. (str "Syntax error: "
                                                               if-node))))]
    {:nodes (if (flip (context-lookup context decision))
              (take-while #(not= "else" (:tag-name %)) body-nodes)
              (rest (drop-while #(not= "else" (:tag-name %)) body-nodes)))}))

(deftemplatetag "ifequal" "endifequal" [nodes context]
  (let [if-node (first nodes)
        operands (:args if-node)
        body-nodes (rest (butlast nodes))]
    {:nodes (if (apply = (for [op operands]
                           (if (= \" (.charAt op 0))
                             (subs op 1 (dec (count op)))
                             (context-lookup context op))))
              (take-while #(not= "else" (:tag-name %)) body-nodes)
              (rest (drop-while #(not= "else" (:tag-name %)) body-nodes)))}))

(deftemplatetag "for" "endfor" [nodes context]
  (let [for-node (first nodes)
        [x in coll] (:args for-node)
        body-nodes (rest (butlast nodes))]
    (if (= in "in")
      (let [coll (context-lookup context coll)
            single? (= 1 (count coll))]
        (when (seq coll)
          {:groups (cons {:nodes body-nodes
                          :context (assoc context
                                     (keyword x) (first coll)
                                     :forloop {:first true
                                               :last single?})}
                         (when-not single?
                           (concat (for [ele (rest (butlast coll))]
                                     {:nodes body-nodes
                                      :context (assoc context
                                                 (keyword x) ele
                                                 :forloop {:first false
                                                           :last false})})
                                   [{:nodes body-nodes
                                     :context (assoc context
                                                (keyword x) (last coll)
                                                :forloop {:first false
                                                          :last true})}])))}))
      (throw (Exception. (str "syntax error in:" for-node))))))
