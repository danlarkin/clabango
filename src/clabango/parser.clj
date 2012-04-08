(ns clabango.parser
  (:use [clabango.filters :only [template-filter]]
        [clabango.tags :only [load-template template-tag valid-tags]]))

(declare lex* parse ast->parsed)

(defn start-of-new-token? [s i]
  (let [c (.charAt s i)
        nc (try
             (.charAt s (inc i))
             (catch StringIndexOutOfBoundsException _))]
    (or (= c \newline)
        (and (= c \{)
             (or (= nc \{)
                 (= nc \%)))
        (and (= c \%)
             (= nc \}))
        (and (= c \})
             (= nc \})))))

(defn buffer-string [s fileref i max offset line]
  (let [sb (StringBuffer.)]
    (loop [ni i]
      (if (or (>= ni max)
              (start-of-new-token? s ni))
        (cons {:token (str sb)
               :offset offset
               :line line
               :file fileref}
              (lex* s fileref ni max (+ offset (- ni i)) line))
        (do
          (.append sb (.charAt s ni))
          (recur (inc ni)))))))

(defn lex* [s fileref i max offset line]
  (lazy-seq
   (when (< i max)
     (let [c (.charAt s i)
           nc (try
                (.charAt s (inc i))
                (catch StringIndexOutOfBoundsException _))]
       (case c
         \{ (case nc
              \{ (cons {:token :open-filter
                        :offset offset
                        :line line
                        :file fileref}
                       (lex* s fileref (+ i 2) max (+ offset 2) line))
              \% (cons {:token :open-tag
                        :offset offset
                        :line line
                        :file fileref}
                       (lex* s fileref (+ i 2) max (+ offset 2) line))
              (buffer-string s fileref i max offset line))
         \} (case nc
              \} (cons {:token :close-filter
                        :offset offset
                        :line line
                        :file fileref}
                       (lex* s fileref (+ i 2) max (+ offset 2) line))
              (buffer-string s fileref i max offset line))
         \% (case nc
              \} (cons {:token :close-tag
                        :offset offset
                        :line line
                        :file fileref}
                       (lex* s fileref (+ i 2) max (+ offset 2) line))
              (buffer-string s fileref i max offset line))
         \newline (cons {:token "\n"
                         :offset offset
                         :line line
                         :file fileref}
                        (lex* s fileref (inc i) max 1 (inc line)))
         (buffer-string s fileref i max offset line))))))

(defn lex [string-or-file]
  (let [[s fileref] (if (string? string-or-file)
                      [string-or-file "UNKNOWN"]
                      [(slurp string-or-file) string-or-file])
        max (.length s)]
    (lex* s fileref 0 max 1 1)))

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

(defn valid-tag? [tag-name]
  (let [valid-tags-snapshot @valid-tags]
    (or (valid-tags-snapshot tag-name)
        ((clojure.set/difference (set (vals valid-tags-snapshot))
                                 #{:inline})
         tag-name))))

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
           (let [[body end-node & rest-ast] (partition-by
                                             (partial tag-is? end-tag-name)
                                             ast)
                 body (conj (vec body) end-node)
                 rest-ast (flatten rest-ast)]
             (let [[s-or-nodes new-context] (template-tag (:tag-name node) body
                                                          context)]
               (concat (if-not (coll? s-or-nodes)
                         (parse s-or-nodes new-context)
                         ;; TODO: this will go all the way to realizing vars
                         ;; which is probably too much, for instance if the
                         ;; block tag wants to set something in the context
                         ;; and then include another template, does realizing
                         ;; vars here break that or make it possible?
                         (ast->parsed s-or-nodes new-context))
                       (interpret-tags rest-ast context))))
           (let [[s-or-nodes new-context] (template-tag (:tag-name node) [node]
                                                        context)]
             (concat (if-not (coll? s-or-nodes)
                       (parse s-or-nodes new-context)
                       (ast->parsed s-or-nodes new-context))
                     (interpret-tags (rest ast) new-context)))))
       (cons node (interpret-tags (rest ast) context))))))

(defn get-block-overrides [ast]
  (loop [ast ast
         current-block-metadata {}
         block-overrides {}
         block-metadata {}]
    (if-let [node (first ast)]
      (if-let [block-name (:block-name node)]
        (if (and (block-overrides block-name)
                 (= (:block-metadata node) current-block-metadata))
          (recur (rest ast)
                 current-block-metadata
                 (update-in block-overrides [block-name] conj node)
                 block-metadata)
          (recur (rest ast)
                 (:block-metadata node)
                 (assoc block-overrides block-name [node])
                 (assoc block-metadata block-name (:block-metadata node))))
        (recur (rest ast)
               current-block-metadata
               block-overrides
               block-metadata))
      [block-overrides block-metadata])))

(defn reduce-blocks [ast]
  (let [[block-overrides block-metadata] (get-block-overrides ast)]
    (loop [result []
           ast ast
           filled-blocks #{}]
      (if-let [node (first ast)]
        (if-let [block-name (:block-name node)]
          (if (block-overrides block-name)
            (if-not (filled-blocks block-name)
              (if-not (= (:block-metadata node) (block-metadata block-name))
                (let [replacement-nodes (block-overrides block-name)]
                  (recur (vec (concat result replacement-nodes))
                         (rest ast)
                         (conj filled-blocks block-name)))
                (recur (conj result node)
                       (rest ast)
                       filled-blocks))
              (recur result
                     (rest ast)
                     filled-blocks))
            (recur result
                   (rest ast)
                   filled-blocks))
          (recur (conj result node)
                 (rest ast)
                 filled-blocks))
        result))))

(defn parse-filters [ast context]
  (lazy-seq
   (when-let [node (first ast)]
     (if (= :filter (:type node))
       ;; TODO: is turning var lookups into keywords a good idea?
       ;; the alternative is leaving them as strings?
       (let [var-and-filter (.trim (:token (:body node)))
             [var filter-name] (rest (re-find #"(.*)\|(.*)" var-and-filter))]
         (cons (-> node
                   (assoc :type :string)
                   (assoc-in [:body :token]
                             (if filter-name
                               (template-filter filter-name
                                                (str (context (keyword var))))
                               (str (context (keyword var-and-filter))))))
               (parse-filters (rest ast) context)))
       (cons node (parse-filters (rest ast) context))))))

(defn realize [ast]
  (let [sb (StringBuilder.)]
    (loop [ast ast]
      (if-let [node (first ast)]
        (case (:type node)
          :string (do
                    (.append sb (:token (:body node)))
                    (recur (rest ast)))
          :noop (recur (rest ast))
          (throw (Exception.
                  (str "there should only be AST nodes of type"
                       " :string and :noop, got: "
                       node))))
        (str sb)))))

(defn string->ast [s]
  (-> s
      lex
      ast))

(defn ast->parsed [ast context]
  (-> ast
      parse-tags
      (interpret-tags context)
      reduce-blocks
      (parse-filters context)))

(defn parse [s context]
  (-> s
      string->ast
      (ast->parsed context)))

(def render (comp realize parse))

(defn render-file [filename context]
  (-> filename
      load-template
      (render context)))
