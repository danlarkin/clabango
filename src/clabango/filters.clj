(ns clabango.filters
  (:require [cheshire.core :as cheshire])
  (:import (org.apache.commons.codec.digest DigestUtils)
           (org.joda.time Instant)
           (org.joda.time.format DateTimeFormat)))

(defn context-lookup
  ([context var]
     (context-lookup context var nil))
  ([context var default]
     (get-in context (map keyword (.split var "\\.")) default)))

(defmulti template-filter (fn [filter-name _ _ _] filter-name))

(defn fix-args
  [fn-tail]
  (let [[f & r] fn-tail]
    `(~(vec (cons '_ f))
      ~@r)))

(defmacro deftemplatefilter [filter-name & fn-tail]
  `(defmethod template-filter ~filter-name ~@(fix-args fn-tail)))

(deftemplatefilter "upper" [node body arg]
  (when body
    {:body (.toUpperCase body)}))

(def get-date-formatter
  (memoize (fn [s]
             (DateTimeFormat/forPattern s))))

;; http://joda-time.sourceforge.net/
;; apidocs/org/joda/time/format/DateTimeFormat.html
(deftemplatefilter "date" [node body arg]
  (when body
    (if (not arg)
      (throw (Exception.
              (str "date filter requires a format string argument " node)))
      {:body (.print (get-date-formatter (subs arg 1 (dec (count arg))))
                     (Instant. body))})))

(deftemplatefilter "hash" [node body arg]
  (when body
    (if (not arg)
      (throw (Exception.
              (str "hash filter requires a hash name argument " node)))
      (let [hash-name (subs arg 1 (dec (count arg)))]
        {:body (case hash-name
                 "md5" (DigestUtils/md5Hex body)
                 "sha" (DigestUtils/shaHex body)
                 "sha256" (DigestUtils/sha256Hex body)
                 "sha384" (DigestUtils/sha384Hex body)
                 "sha512" (DigestUtils/sha512Hex body)
                 (throw (Exception.
                         hash-name (str "is not a valid hash algorithm "
                                        node))))}))))

(deftemplatefilter "count" [node body arg]
  {:body (if body
           (str (count body))
           "0")})

(defn pluralize-suffixes [arg]
  (if arg
    (let [no-quotes (subs arg 1 (dec (count arg)))
          f (.split no-quotes "," 2)]
      (if (= 2 (count f))
        f
        [nil (first f)]))
    [nil "s"]))

(deftemplatefilter "pluralize" [node body arg]
  (when body
    (let [[single plural] (pluralize-suffixes arg)]
      {:body (if (= (count body) 1)
               single
               plural)})))

(deftemplatefilter "to-json" [node body arg]
  {:body (if body
           (cheshire/encode body)
           "null")})

(deftemplatefilter "safe" [node body arg]
  {:body body
   :safe? true})
