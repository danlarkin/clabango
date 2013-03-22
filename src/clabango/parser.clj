(ns clabango.parser
  (:require [clojure.set]
            [clabango.filters :refer [context-lookup template-filter]]
            [clabango.tags :refer [get-block-status load-template
                                   template-tag valid-tags]]))

(defn escape-html
  "Change special characters into HTML character entities."
  [text]
  (.. ^String (as-str text)
    (replace "&"  "&amp;")
    (replace "<"  "&lt;")
    (replace ">"  "&gt;")
    (replace "\"" "&quot;")))

(declare lex* string->ast ast->groups)

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
       (let [[tag & args] (map first (re-seq #"[^\s\"']+|\"([^\"]*)\"|'([^']*)'"
                                             (.trim (:token (:body node)))))]
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

(defn find-body [end-tag-name ast]
  (let [start-tag-name (:tag-name (first ast))]
    (loop [i -1 ;; first tag is the opener so start "one down"
           body []
           ast ast]
      (when-let [node (first ast)]
        (cond
         (and (= (:tag-name node) end-tag-name) (zero? i))
         [(conj body node) (rest ast)]

         (= (:tag-name node) end-tag-name)
         (recur (dec i)
                (conj body node)
                (rest ast))

         (= (:tag-name node) start-tag-name)
         (recur (inc i)
                (conj body node)
                (rest ast))

         :default
         (recur i
                (conj body node)
                (rest ast)))))))

(defn interpret-tags [ast context]
  (lazy-seq
   (when-let [node (first ast)]
     (case (:type node)
       :tag (let [end-tag-name (valid-tag? (:tag-name node))
                  [body rest-ast] (if (or (nil? end-tag-name)
                                          (= end-tag-name :inline))
                                    [[node]
                                     (rest ast)]
                                    (find-body end-tag-name ast))
                  tag-result (template-tag (:tag-name node) body context)]
              (concat (cond
                       (:string tag-result)
                       (-> (:string tag-result)
                           string->ast
                           (ast->groups (:context tag-result context)))

                       (:nodes tag-result)
                       (ast->groups (:nodes tag-result)
                                    (:context tag-result context))

                       (:groups tag-result)
                       (apply concat (for [group (:groups tag-result)]
                                       (ast->groups (:nodes group)
                                                    (:context group context)))))
                      (interpret-tags rest-ast context)))

       :block (cons (update-in node [:nodes] #(ast->groups % context))
                    (interpret-tags (rest ast) context))

       (cons node (interpret-tags (rest ast) context))))))

(defn parse-filters [ast context]
  (lazy-seq
   (when-let [node (first ast)]
     (case (:type node)
       :filter
       (let [var-and-filter (.trim (:token (:body node)))
             [var filter-name arg] (rest (re-find #"([^|]+)\|([^:]+):?(.+)?"
                                                  var-and-filter))]
         (if (and arg
                  (not (and (.startsWith arg "\"")
                            (.endsWith arg "\""))))
           (throw (Exception. (str "filter arguments must be in quotes " node)))
           (cons (-> node
                     (assoc :type :string)
                     (assoc-in
                      [:body :token]
                      (str (if filter-name
                             (template-filter
                              filter-name
                              node
                              (context-lookup context var)
                              arg)
                             (context-lookup context var-and-filter)))))
                 (parse-filters (rest ast) context))))

       :group
       ;; not a tail call, I know
       (cons (update-in node [:nodes] #(parse-filters % context))
             (parse-filters (rest ast) context))

       (cons node (parse-filters (rest ast) context))))))

(defn get-block-values [ast]
  (loop [ast (reverse ast)
         block-values {}]
    (if-let [node (first ast)]
      (let [block-name (:name node)]
        (if (and (= :block (:type node))
                 (not (block-values block-name)))
          (recur (rest ast)
                 (assoc block-values block-name node))
          (recur (rest ast)
                 block-values)))
      block-values)))

(defn reduce-blocks* [ast block-values]
  (loop [ast ast
         result []
         filled-blocks #{}]
    (if-let [node (first ast)]
      (if (= :block (:type node))
        (let [block-name (:name node)]
          (if-not (filled-blocks block-name)
            (let [block-nodes (:nodes (block-values block-name))
                  ;; more non-tail calls, I know
                  sub-reduction (reduce-blocks*
                                 block-nodes
                                 (merge (get-block-values block-nodes)
                                        block-values))]
              (recur (rest ast)
                     (vec (concat result (:result sub-reduction)))
                     (-> filled-blocks
                         (into (:filled-blocks sub-reduction))
                         (conj block-name))))
            (recur (rest ast)
                   result
                   filled-blocks)))
        (recur (rest ast)
               (conj result node)
               filled-blocks))
      {:result result
       :filled-blocks filled-blocks})))

(defn reduce-blocks [ast]
  (:result (reduce-blocks* ast (get-block-values ast))))

(defn realize [ast]
  (let [sb (StringBuilder.)]
    (loop [ast ast]
      (if-let [node (first ast)]
        (case (:type node)
          :string (do
                    (.append sb (escape-html (:token (:body node))))
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

(defn ast->groups [ast context]
  (-> ast
      parse-tags
      (interpret-tags context)
      (parse-filters context)))

(defn groups->parsed [ast]
  (-> ast
      reduce-blocks))

(defn parse [s context]
  (-> s
      string->ast
      (ast->groups context)
      groups->parsed))

(def render (comp realize parse))

(defn render-file [filename context]
  (-> filename
      load-template
      (render context)))
