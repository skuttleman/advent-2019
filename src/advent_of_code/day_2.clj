(ns advent-of-code.day-2
  (:require [advent-of-code.shared.int-code :as int-code]
            [advent-of-code.shared.utils :as u]))

(defn pos-one [v noun verb]
  (first (int-code/compute (assoc v 1 noun 2 verb))))

(comment
  (let [input (u/int-code-resource 2)]
    ;; part one
    (pos-one input 12 2)

    ;; part two
    (first (for [noun (range 100) verb (range 100)
                 :when (= 19690720 (pos-one input noun verb))]
             (+ (* 100 noun) verb)))))
