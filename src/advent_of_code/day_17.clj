(ns advent-of-code.day-17
  (:require [advent-of-code.shared.directions :as dir]
            [advent-of-code.shared.int-code :as int-code]
            [advent-of-code.shared.utils :as u]
            [clojure.core.async :as async]))

(defn output [input]
  (let [in (async/chan 100 (map int))
        out (async/chan 100 (map char))]
    (int-code/compute input in out)
    (async/go-loop [x 0 y 0 grid {}]
      (if-let [n (async/<! out)]
        (if (= \newline n)
          (recur 0 (inc y) grid)
          (recur (inc x) y (assoc grid [x y] n)))
        grid))))

(def robot-dir
  {\^ :up
   \v :down
   \< :left
   \> :right})

(def scaffold? (into #{\#} (keys robot-dir)))

(defn intersections [grid]
  (for [pos (keys grid)
        :when (and (scaffold? (get grid pos))
                   (scaffold? (get grid (dir/next-pos pos :up)))
                   (scaffold? (get grid (dir/next-pos pos :down)))
                   (scaffold? (get grid (dir/next-pos pos :left)))
                   (scaffold? (get grid (dir/next-pos pos :right))))]
    pos))

(comment
  (let [input (u/int-code-resource 17)]
    ;; part one
    (->> (output input)
         async/<!!
         intersections
         (transduce (map (partial apply *)) +))))
