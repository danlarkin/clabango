(ns clabango.filters)

(defmulti template-filter (fn [filter-name _] filter-name))

(defmethod template-filter "upper" [_ body]
  (.toUpperCase body))
