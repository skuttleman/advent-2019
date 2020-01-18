(ns advent-of-code.shared.utils
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
  (Long/parseLong (str s)))

(defn comma-separated [xform]
  (fn [line]
    (sequence xform (string/split line #","))))

(defn combos [set]
  (cond
    (empty? set) ()
    (= 1 (count set)) (map list set)
    :else (mapcat (fn [item]
                    (map (partial cons item) (combos (disj set item))))
                  set)))

(defn int-code-resource [day]
  (first (resource day (map (comp vec (comma-separated (map parse-long)))))))
