(ns advent-of-code.day-1
  (:require [advent-of-code.utils :as u]))

(defn mass->fuel [mass]
  (- (long (Math/floor (/ mass 3))) 2))

(defn mass->fuel' [mass]
  (loop [fuel 0 mass mass]
    (let [next-fuel (mass->fuel mass)]
      (if (pos? next-fuel)
        (recur (+ fuel next-fuel) next-fuel)
        fuel))))

(comment
  (let [input (u/resource 1 (map u/parse-long))]
    ;; part one
    (transduce (map mass->fuel) + input)

    ;; part two
    (transduce (map mass->fuel') + input)))
