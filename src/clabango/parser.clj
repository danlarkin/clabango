(ns clabango.parser
  (:use [clabango.filters :only [template-filter]]
        [clabango.tags :only [template-tag]]))

(declare parse)

(defn lex [s]
  ;; this is a doozy, eh?
  (let [max (.length s)
        sb (StringBuilder.)]
    (loop [result []
           started 1
           i 0]
      (if (>= i max)
        (if (zero? (.length sb))
          result
          (conj result {:started started
                        :token (str sb)}))
        (let [c (.charAt s i)]
          (if (>= (inc i) max)
            (do
              (.append sb c)
              (recur result
                     started
                     (inc i)))
            (case c
              \{ (let [ni (+ 2 i)
                       nc (.charAt s (inc i))]
                   (case nc
                     \{ (let [s (str sb)
                              slen (.length sb)
                              new-started (+ started slen)]
                          (.delete sb 0 slen)
                          (recur (if (zero? slen)
                                   (conj result {:started new-started
                                                 :token :open-filter})
                                   (vec (concat result
                                                [{:started started
                                                  :token s}
                                                 {:started new-started
                                                  :token :open-filter}])))
                                 (+ 2 new-started)
                                 ni))
                     \% (let [s (str sb)
                              slen (.length sb)
                              new-started (+ started slen)]
                          (.delete sb 0 slen)
                          (recur (if (zero? slen)
                                   (conj result {:started new-started
                                                 :token :open-tag})
                                   (vec (concat result [{:started started
                                                         :token s}
                                                        {:started new-started
                                                         :token :open-tag}])))
                                 (+ 2 new-started)
                                 ni))
                     (do
                       (.append sb c)
                       (.append sb nc)
                       (recur result
                              started
                              ni))))
              \} (let [ni (+ 2 i)
                       nc (.charAt s (inc i))]
                   (case nc
                     \} (let [s (str sb)
                              slen (.length sb)
                              new-started (+ started slen)]
                          (.delete sb 0 slen)
                          (recur (if (zero? slen)
                                   (conj result {:started new-started
                                                 :token :close-filter})
                                   (vec (concat result
                                                [{:started started
                                                  :token s}
                                                 {:started new-started
                                                  :token :close-filter}])))
                                 (+ 2 new-started)
                                 ni))
                     (do
                       (.append sb c)
                       (.append sb nc)
                       (recur result
                              started
                              ni))))
              \% (let [ni (+ 2 i)
                       nc (.charAt s (inc i))]
                   (case nc
                     \} (let [s (str sb)
                              slen (.length sb)
                              new-started (+ started slen)]
                          (.delete sb 0 slen)
                          (recur (if (zero? slen)
                                   (conj result {:started new-started
                                                 :token :close-tag})
                                   (vec (concat result [{:started started
                                                         :token s}
                                                        {:started new-started
                                                         :token :close-tag}])))
                                 (+ 2 new-started)
                                 ni))
                     (do
                       (.append sb c)
                       (.append sb nc)
                       (recur result
                              started
                              ni))))
              (do
                (.append sb c)
                (recur result
                       started
                       (inc i))))))))))

(defn find-close-filter [tokens]
  (let [[a b c & rest-tokens] tokens]
    (if (and (= (:token a) :open-filter)
             (string? (:token b))
             (= (:token c) :close-filter))
      [{:type :filter
        :body b}
       rest-tokens]
      (throw (Exception. (str "parsing error after " a))))))

(defn find-close-tag [tokens]
  (let [[a b c & rest-tokens] tokens]
    (if (and (= (:token a) :open-tag)
             (string? (:token b))
             (= (:token c) :close-tag))
      [{:type :tag
        :body b}
       rest-tokens]
      (throw (Exception. (str "parsing error after " a))))))

(defn ast [tokens]
  (lazy-seq
   (when-let [token (first tokens)]
     (case (:token token)
       :open-filter (let [[token rest-tokens] (find-close-filter tokens)]
                      (cons token (ast rest-tokens)))
       :open-tag (let [[token rest-tokens] (find-close-tag tokens)]
                   (cons token (ast rest-tokens)))
       (cons {:type :string
              :body token}
             (ast (rest tokens)))))))

(def valid-tags {"include" :inline
                 "block" "endblock"})

(defn valid-tag? [tag-name]
  (or (valid-tags tag-name)
      (when-let [start-tag-name (second (re-find #"end(.*)" tag-name))]
        (valid-tags start-tag-name))))

(defn parse-tags [ast]
  (lazy-seq
   (when-let [node (first ast)]
     (if (= :tag (:type node))
       (let [[tag & args] (.split (.trim (:token (:body node))) " ")]
         (if (valid-tag? tag)
           (cons (assoc node
                   :tag-name tag
                   :args args)
                 (parse-tags (rest ast)))
           (throw (Exception. (str "unknown tag: " node)))))
       (cons node (parse-tags (rest ast)))))))

(defn find-close-tag-for [tokens tag-name]
  (let [[a b c & rest-tokens] tokens]
    (if (and (= (:token a) :open-tag)
             (string? (:token b))
             (= (:token c) :close-tag))
      [{:type :tag
        :body b}
       rest-tokens]
      (throw (Exception. (str "parsing error after " a))))))

(defn tag-is? [tag-name node]
  (= (:tag-name node) tag-name))

(defn interpret-tags [ast context]
  (lazy-seq
   (when-let [node (first ast)]
     (if (= :tag (:type node))
       (let [end-tag-name (valid-tag? (:tag-name node))]
         (if-not (or (nil? end-tag-name)
                     (= end-tag-name :inline))
           (let [[body end-node rest-ast] (partition-by
                                           (partial tag-is? end-tag-name)
                                           ast)
                 body (conj (vec body) end-node)]
             (let [[s new-context] (template-tag (:tag-name node) body context)]
               (concat (parse s new-context)
                       (interpret-tags rest-ast new-context))))
           (let [[s new-context] (template-tag (:tag-name node) [node] context)]
             (concat (parse s new-context)
                     (interpret-tags (rest ast) new-context)))))
       (cons node (interpret-tags (rest ast) context))))))

(defn parse-filters [ast context]
  (lazy-seq
   (when-let [node (first ast)]
     (if (= :filter (:type node))
       ;; TODO: is turning var lookups into keywords a good idea?
       ;; the alternative is leaving them as strings?
       (let [var-and-filter (.trim (:token (:body node)))
             [var filter-name] (rest (re-find #"(.*)\|(.*)" var-and-filter))]
         (cons {:type :string
                :body {:started (:started (:body node))
                       :token (if filter-name
                                (template-filter filter-name
                                                 (str (context (keyword var))))
                                (str (context (keyword var-and-filter))))}}
               (parse-filters (rest ast) context)))
       (cons node (parse-filters (rest ast) context))))))

(defn realize [ast]
  (let [sb (StringBuilder.)]
    (loop [ast ast]
      (if-let [node (first ast)]
        (if (= :string (:type node))
          (do
            (.append sb (:token (:body node)))
            (recur (rest ast)))
          (throw (Exception.
                  (str "there should only be AST nodes of type :string, got: "
                       node))))
        (str sb)))))

(defn parse [s context]
  (-> s
      lex
      ast
      parse-tags
      (interpret-tags context)
      (parse-filters context)))

(def render (comp realize parse))
