(ns clabango.filters)

(defn context-lookup
  ([context var]
     (context-lookup context var nil))
  ([context var default]
     (get-in context (map keyword (.split var "\\.")) default)))

(defmulti template-filter (fn [filter-name _] filter-name))

(defmethod template-filter "upper" [_ body]
  (.toUpperCase body))
