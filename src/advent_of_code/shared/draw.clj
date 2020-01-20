(ns advent-of-code.shared.draw
  (:require [clojure.string :as string]))

(defn draw-line [grid draw-square left right y]
  (for [x (range left (inc right))]
    (draw-square x y (grid [x y]))))

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

(defn draw [grid draw-square]
  (let [{:keys [top left bottom right]} (boundaries (keys grid))]
    (->> (range top (inc bottom))
         (map (partial draw-line grid draw-square left right))
         (map string/join)
         (string/join "\n"))))
