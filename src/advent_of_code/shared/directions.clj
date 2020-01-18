(ns advent-of-code.shared.directions)

(def turn-left
  {:up    :left
   :left  :down
   :down  :right
   :right :up})

(def turn-right
  {:up    :right
   :right :down
   :down  :left
   :left  :up})

(defn next-pos [[x y] direction]
  (case direction
    :left [(dec x) y]
    :right [(inc x) y]
    :up [x (dec y)]
    :down [x (inc y)]))
