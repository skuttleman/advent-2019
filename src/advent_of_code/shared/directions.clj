(ns advent-of-code.shared.directions)

(def turn-left
  {:up    :left
   :down  :right
   :left  :down
   :right :up})

(def turn-right
  {:up    :right
   :down  :left
   :left  :up
   :right :down})

(defn next-pos [[x y] direction]
  (case direction
    :left [(dec x) y]
    :right [(inc x) y]
    :up [x (dec y)]
    :down [x (inc y)]))

(defn direction [[x-1 y-1] [x-2 y-2]]
  (cond
    (< x-1 x-2) :right
    (> x-1 x-2) :left
    (< y-1 y-2) :down
    (> y-1 y-2) :up))
