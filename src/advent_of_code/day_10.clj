(ns advent-of-code.day-10
  (:require [advent-of-code.shared.utils :as u]))

(defn find-asteroids [y s]
  (->> s
       (map-indexed vector)
       (filter (comp #{\#} second))
       (map #(vector (first %) y))))

(defn distance [[x-1 y-1] [x-2 y-2]]
  (Math/sqrt (+ (Math/pow (- x-2 x-1) 2)
                (Math/pow (- y-2 y-1) 2))))

(defn find-angle [[x-1 y-1] [x-2 y-2]]
  (-> (Math/atan2 (- y-2 y-1) (- x-1 x-2))
      (* 180)
      (/ (- Math/PI))
      (+ 270)
      (mod 360)))

(defn between? [asteroid-1 angle dist asteroid-2]
  (and (= angle (find-angle asteroid-1 asteroid-2))
       (> dist (distance asteroid-1 asteroid-2))))

(defn count-los [asteroids asteroid]
  (let [asteroids' (disj asteroids asteroid)]
    (count (for [other asteroids'
                 :let [angle (find-angle asteroid other)
                       dist (distance asteroid other)]
                 :when (empty? (filter (partial between? asteroid angle dist) (disj asteroids' other)))]
             other))))

(defn most-los [input]
  (->> input
       (map (partial count-los input))
       (sort >)
       first))

(defn destroy [n [[_ angle] :as asteroids]]
  (if (and (seq asteroids) (pos? n))
    (let [[beginning end] (split-with #(== (second %) angle) (rest asteroids))]
      (recur (dec n) (concat end beginning)))
    asteroids))

(defn x*y [[x y]]
  (+ y (* 100 x)))

(defn nth-destroyed [input most-los n]
  (->> (disj input most-los)
       (map (juxt identity (partial find-angle most-los) (partial distance most-los)))
       (sort-by (comp vec rest))
       (destroy (dec n))
       ffirst
       x*y))

(comment
  (let [input (set (u/resource 10 (comp (map-indexed find-asteroids) cat)))]
    ;; part one
    ;; O(n^2) 😞
    (most-los input)

    ;; part two
    (nth-destroyed input [20 19] 200)))
