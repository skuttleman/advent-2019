(ns advent-of-code.day-3
  (:require [advent-of-code.utils :as u]
            [clojure.set :as set]))

(defn parse-inst [s]
  (let [n (u/parse-long (subs s 1))]
    (case (first s)
      \R [:right n]
      \U [:up n]
      \D [:down n]
      \L [:left n])))

(defn next-pos [[x y] direction]
  (case direction
    :left [(dec x) y]
    :right [(inc x) y]
    :up [x (dec y)]
    :down [x (inc y)]))

(def key-set (comp set keys))

(defn pos-intersection [grid-1 grid-2]
  (set/intersection (key-set grid-1) (key-set grid-2)))

(defn manhattan [[x y]]
  (+ (Math/abs ^long x) (Math/abs ^long y)))

(defn min-steps [grid-1 grid-2]
  (fn [k]
    (+ (get grid-1 k) (get grid-2 k))))

(defn update-grid [grid pos steps]
  (update grid pos #(or % steps)))

(defn consume-inst [grid pos steps [direction n]]
  (if (zero? n)
    [grid pos steps]
    (let [pos' (next-pos pos direction)
          steps' (inc steps)]
      (recur (update-grid grid pos' steps') pos' steps' [direction (dec n)]))))

(defn consume-line [line]
  (loop [grid {} pos [0 0] steps 0 [inst :as line] line]
    (if (empty? line)
      grid
      (let [[grid' pos' steps'] (consume-inst grid pos steps inst)]
        (recur grid' pos' steps' (rest line))))))

(defn find-best [grid-1 grid-2 mapper]
  (->> (pos-intersection grid-1 grid-2)
       (map (mapper grid-1 grid-2))
       sort
       first))

(comment
  (let [[line-1 line-2] (u/resource 3 (map (u/comma-separated (map parse-inst))))]
    ;; part one
    (find-best (consume-line line-1) (consume-line line-2) (constantly manhattan))

    ;; part two
    (find-best (consume-line line-1) (consume-line line-2) min-steps)))
