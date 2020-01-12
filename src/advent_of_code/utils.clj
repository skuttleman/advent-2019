(ns advent-of-code.utils
  (:require [clojure.java.io :as io]))

(defn resource
  ([day]
   (resource day identity))
  ([day xform]
   (->> (io/resource (str "day-" day ".txt"))
        io/reader
        line-seq
        (sequence xform))))

(defn parse-long [s]
  (Long/parseLong s))
