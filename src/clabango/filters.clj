(ns clabango.filters)

(defmulti template-filter (fn [filter _] filter))

(defmethod template-filter "upper" [_ body]
  (.toUpperCase body))
