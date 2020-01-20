(ns advent-of-code.day-11
  (:require [advent-of-code.shared.directions :as dir]
            [advent-of-code.shared.draw :as draw]
            [advent-of-code.shared.int-code :as int-code]
            [advent-of-code.shared.utils :as u]
            [clojure.core.async :as async]
            [clojure.string :as string]))

(defn output [input start-color]
  (let [in (async/chan 100)
        out (async/chan 100)]
    (int-code/compute input in out)
    (async/go-loop [direction :up pos [0 0] painted {[0 0] start-color}]
      (async/>! in (painted pos 0))
      (if-let [color (async/<! out)]
        (let [turn (async/<! out)
              next-dir (if (zero? turn)
                         (dir/turn-left direction)
                         (dir/turn-right direction))]
          (recur next-dir (dir/next-pos pos next-dir) (assoc painted pos color)))
        painted))))

(defn draw-square [_ _ value]
  (if (zero? (or value 0))
    \space
    \X))

(defn draw [grid]
  (draw/draw grid draw-square))

(comment
  (let [input (u/int-code-resource 11)]
    ;; part one
    (count (async/<!! (output input 0)))

    ;; part two
    (println (draw (async/<!! (output input 1))))))
