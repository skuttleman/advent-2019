(ns advent-of-code.day-8
  (:require [advent-of-code.shared.utils :as u]
            [clojure.string :as string]))

(defn ->layers [width height digits]
  (partition (* width height) digits))

(defn ->rows [width digits]
  (partition width digits))

(defn ->pixel [& pixels]
  (loop [[px & pxs] pixels]
    (cond
      ;(empty? pxs) 0
      (#{0 1} px) px
      :else (recur pxs))))

(def ->print-digit
  {0 \_
   1 \W})

(defn ->str [rows]
  (->> rows
       (map (partial transduce (map ->print-digit) str))
       (string/join "\n")))

(defn get-count [digit freq]
  (get freq digit 0))

(defn one-by-two [freq]
  (* (get-count 1 freq) (get-count 2 freq)))

(defn least-zeros [input width height]
  (->> input
       (->layers width height)
       (map frequencies)
       (sort-by (partial get-count 0))
       first
       one-by-two))

(defn decode-msg [input width height]
  (->> input
       (->layers width height)
       (apply map ->pixel)
       (->rows width)
       ->str
       println))

(comment
  (let [input (first (u/resource 8 (map (comp (partial map u/parse-long) seq))))]
    ;; part one
    (least-zeros input 25 6)

    ;; part two
    (-> input
        (decode-msg 25 6)
        ->str
        println)))
