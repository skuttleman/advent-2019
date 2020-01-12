(ns advent-of-code.day-2
  (:require [advent-of-code.utils :as u]
            [clojure.string :as string]))

(defn operand [v pos operand-num]
  (->> pos
       (+ operand-num)
       (get v)
       (get v)))

(defn target [v pos]
  (get v (+ 3 pos)))

(defn step* [f v pos]
  (assoc v (target v pos) (f (operand v pos 1) (operand v pos 2))))

(defn step [v pos]
  (let [val (get v pos)]
    (case val
      99 (with-meta v {:done? true})
      1 (step* + v pos)
      2 (step* * v pos))))

(defn step-through [v]
  (loop [pos 0 v v]
    (let [next-v (step v pos)]
      (if (:done? (meta next-v))
        next-v
        (recur (+ 4 pos) next-v)))))

(defn pos-one [v noun verb]
  (first (step-through (assoc v 1 noun 2 verb))))

(comment
  (let [input (first (u/resource 2 (map (comp vec (u/comma-separated (map u/parse-long))))))]
    ;; part one
    (pos-one input 12 2)

    ;; part two
    (first (for [noun (range 100) verb (range 100)
                 :when (= 19690720 (pos-one input noun verb))]
             (+ (* 100 noun) verb)))))
