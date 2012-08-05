(ns clabango.filters
  (:import (org.joda.time Instant)
           (org.joda.time.format DateTimeFormat)))

(defn context-lookup
  ([context var]
     (context-lookup context var nil))
  ([context var default]
     (get-in context (map keyword (.split var "\\.")) default)))

(defmulti template-filter (fn [filter-name _ _ _] filter-name))

(defmethod template-filter "upper" [_ node body arg]
  (when body
    (.toUpperCase body)))

(def get-date-formatter
  (memoize (fn [s]
             (DateTimeFormat/forPattern s))))

;; http://joda-time.sourceforge.net/
;; apidocs/org/joda/time/format/DateTimeFormat.html
(defmethod template-filter "date" [_ node body arg]
  (when body
    (if (not arg)
      (throw (Exception.
              (str "date filter requires a format string argument " node)))
      (.print (get-date-formatter (subs arg 1 (dec (count arg))))
              (Instant. body)))))
