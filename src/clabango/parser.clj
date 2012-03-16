(ns clabango.parser)

(def lex seq)

(defn parse-for [tokens desired-tokens]
  (loop [acc []
         tokens tokens]
    (if (not (seq tokens))
      [acc nil]
      (if (= desired-tokens (take (count desired-tokens) tokens))
        (with-meta
          [acc (nthnext tokens (count desired-tokens))]
          {:matched true})
        (recur (conj acc (first tokens))
               (next tokens))))))

(defn parse-var [tokens]
  (let [[a b & rest-tokens] tokens]
    (when (= a b \{)
      (let [[body parsed-rest :as t] (parse-for rest-tokens [\} \}])
            matched? (:matched (meta t))]
        (if (and matched?
                 (not ((set body) \|)))
          [body
           parsed-rest]
          nil)))))

(defn parse-filter [tokens]
  (let [[a b & rest-tokens] tokens]
    (when (= a b \{)
      (let [[body parsed-rest :as t] (parse-for rest-tokens [\} \}])
            matched? (:matched (meta t))]
        (if (and matched?)
          [(concat body (seq "AW YEAH"))
           parsed-rest]
          nil)))))

(def balanced-tags {"start" (seq "{% stop %}")})

(defn parse-tag [tokens]
  (let [[a b & rest-tokens] tokens]
    (when (and (= a \{)
               (= b \%))
      (let [[body parsed-rest :as t] (parse-for rest-tokens [\% \}])
            body (apply str body)
            matched? (:matched (meta t))]
        (if matched?
          (if-let [end-tag (balanced-tags (.trim body))]
            (let [[body parsed-rest :as t]  (parse-for parsed-rest end-tag)
                  body (apply str body)
                  matched? (:matched (meta t))]
              (when matched?
                [(concat body (seq "<----"))
                 parsed-rest]                ))
            [(concat body (seq "AW YEAH"))
             parsed-rest])
          nil)))))

(defn parse [tokens]
  (loop [acc []
         tokens tokens]
    (if-let [token (first tokens)]
      (if-let [[new-node rest-tokens] (parse-var tokens)]
        (recur (conj acc new-node)
               rest-tokens)
        (if-let [[new-node rest-tokens] (parse-filter tokens)]
          (recur (conj acc new-node)
                 rest-tokens)
          (if-let [[new-node rest-tokens] (parse-tag tokens)]
            (recur (conj acc new-node)
                   rest-tokens)
            (recur (conj acc token)
                   (next tokens)))))
      [(flatten acc) tokens])))
