(ns advent-of-code.day-11
  (:require [advent-of-code.shared.directions :as dir]
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

(defn draw-square [grid x y]
  (if (zero? (grid [x y] 0))
    \space
    \X))

(defn draw-line [grid left right y]
  (for [x (range left (inc right))]
    (draw-square grid x y)))

(defn boundaries [ks]
  (reduce (fn [{:keys [top left bottom right]} [x y]]
            {:top    (min top y)
             :left   (min left x)
             :bottom (max bottom y)
             :right  (max right x)})
          {:top    0
           :left   0
           :bottom 0
           :right  0}
          ks))

(defn draw [grid]
  (let [{:keys [top left bottom right]} (boundaries (keys grid))]
    (->> (range top (inc bottom))
         (map (partial draw-line grid left right))
         (map string/join)
         (string/join "\n"))))

(comment
  (let [input (u/int-code-resource 11)]
    ;; part one
    (count (async/<!! (output input 0)))

    ;; part two
    (println (draw (async/<!! (output input 1))))))
