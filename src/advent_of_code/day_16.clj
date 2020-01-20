(ns advent-of-code.day-16
  (:refer-clojure :exclude [bases])
  (:require [advent-of-code.shared.utils :as u]
            [clojure.string :as string]))

(defn next-value [input base]
  (let [n (str (reduce + (map * input base)))]
    (u/parse-long (subs n (dec (count n))))))

(defn bases [input base]
  (map (comp rest cycle #(mapcat (partial repeat %) base) inc)
       (range (count input))))

(defn stepper [input bases]
  (for [base bases]
    (next-value input base)))

(defn steps [step input bases]
  (if (zero? step)
    input
    (recur (dec step) (stepper input bases) bases)))

(comment
  (let [input (first (u/resource 16 (map u/digits)))]
    ;; part one
    (string/join (take 8 (steps 100 input (bases input [0 1 0 -1]) 0)))))
