(ns advent-of-code.day-2
  (:require [advent-of-code.shared.int-code :as int-code]
            [advent-of-code.utils :as u]))

(defn pos-one [v noun verb]
  (first (int-code/compute (assoc v 1 noun 2 verb))))

(comment
  (let [input (first (u/resource 2 (map (comp vec (u/comma-separated (map u/parse-long))))))]
    ;; part one
    (pos-one input 12 2)

    ;; part two
    (first (for [noun (range 100) verb (range 100)
                 :when (= 19690720 (pos-one input noun verb))]
             (+ (* 100 noun) verb)))))
