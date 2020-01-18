(ns advent-of-code.day-12
  (:require [advent-of-code.shared.utils :as u]))

(defn pos-add [[x-1 y-1 z-1] [x-2 y-2 z-2]]
  [(+ x-1 x-2) (+ y-1 y-2) (+ z-1 z-2)])

(defn next-velocity [[[x-1 y-1 z-1 :as p1] v1] [[x-2 y-2 z-2]]]
  [p1 (cond-> v1
        (> x-2 x-1) (update 0 inc)
        (< x-2 x-1) (update 0 dec)
        (> y-2 y-1) (update 1 inc)
        (< y-2 y-1) (update 1 dec)
        (> z-2 z-1) (update 2 inc)
        (< z-2 z-1) (update 2 dec))])

(defn energy [[^long x ^long y ^long z]]
  (+ (Math/abs x) (Math/abs y) (Math/abs z)))

(defn total-energy [[p v]]
  (* (energy p) (energy v)))

(defn next-velocities* [moon-1 [moon-2 :as other-moons]]
  (if (empty? other-moons)
    moon-1
    (recur (next-velocity moon-1 moon-2) (rest other-moons))))

(defn next-velocities [moons]
  (loop [moons' moons [k :as ks] (keys moons)]
    (if (empty? ks)
      moons'
      (recur (update moons' k next-velocities* (vals (dissoc moons k)))
             (rest ks)))))

(defn next-pos [moons]
  (into {}
        (map (fn [[k [pos velocity]]]
                  [k [(pos-add pos velocity) velocity]]))
        moons))

(def step (comp next-pos next-velocities))

(defn steps [moons n]
  (if (zero? n)
    moons
    (recur (step moons) (dec n))))

(defn ->moons [input]
  (into {}
        (comp (map (fn [p] [p [0 0 0]]))
              (map-indexed vector))
        input))

(comment
  (let [input (u/resource 12 (map (comp (partial mapv (comp u/parse-long second))
                                        (partial re-seq #"(-?\d+)"))))]
    ;; part one
    (-> (->moons input)
        (steps 1000)
        vals
        (->> (transduce (map total-energy) + 0)))))
