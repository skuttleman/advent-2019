(ns advent-of-code.day-6
  (:require [advent-of-code.utils :as u]
            [clojure.string :as string]))

(defn ->orbit-info [s]
  (let [[orbitee orbiter] (string/split s #"\)")]
    [(keyword (string/lower-case orbitee)) (keyword (string/lower-case orbiter))]))

(defn ->orbits [orbit-info]
  (loop [orbits {} [[orbitee orbiter] :as orbit-info] orbit-info]
    (if (empty? orbit-info)
      orbits
      (recur (assoc orbits orbiter orbitee) (rest orbit-info)))))

(defn chain-orbits [orbits]
  (fn count-em [chains orbiter]
    (let [orbitee (get orbits orbiter)
          chain (get chains orbitee)]
      (cond
        (nil? orbitee) (assoc chains orbiter ())
        chain (assoc chains orbiter (conj chain orbitee))
        :else (recur (count-em chains orbitee) orbiter)))))

(defn chains [input]
  (let [orbits (->orbits input)]
    (->> (keys orbits)
         (reduce (chain-orbits orbits) {}))))

(defn total-orbits [input]
  (->> (chains input)
       vals
       (map count)
       (reduce + 0)))

(defn orbital-hops [input]
  (let [{:keys [you san]} (chains input)
        santa-distance (zipmap san (range))]
    (->> you
         (map-indexed (fn [n orbitee]
                        [n (santa-distance orbitee)]))
         (filter second)
         first
         (apply +))))

(comment
  (let [input (u/resource 6 (map ->orbit-info))]
    ;; part one
    (total-orbits input)

    ;; part two
    (orbital-hops input)))
