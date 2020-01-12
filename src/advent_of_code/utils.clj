(ns advent-of-code.utils
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

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

(defn comma-separated [xform]
  (fn [line]
    (sequence xform (string/split line #","))))
