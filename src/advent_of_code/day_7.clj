(ns advent-of-code.day-7
  (:require [clojure.core.async :as async]
            [advent-of-code.shared.int-code :as int-code]
            [advent-of-code.shared.utils :as u]))

(defn ->amp [input in-val]
  (let [in (async/chan 100)
        out (async/chan 100)]
    (int-code/compute input in out)
    (async/put! in in-val)
    (fn [src]
      (let [results (async/chan 100)]
        (async/go-loop []
          (when-let [next (async/<! src)]
            (async/>! in next)
            (recur)))
        (async/go-loop []
          (if-let [next (async/<! out)]
            (do (async/>! results next)
                (recur))
            (async/close! results)))
        results))))

(defn amps [input reducer in-vals]
  (->> in-vals
       (map (partial ->amp input))
       reducer))

(defn ->reduced [ch amp-fns]
  (reduce (fn [ch amp]
            (amp ch))
          ch
          amp-fns))

(defn single-reducer [src-val]
  (fn [amp-fns]
    (->reduced (async/to-chan [src-val]) amp-fns)))

(defn round-robin-reducer [src-val]
  (fn [amp-fns]
    (let [loop-ch (async/chan 100)
          reduced-ch (->reduced loop-ch amp-fns)]
      (async/go-loop [val src-val]
        (async/>! loop-ch val)
        (if-let [next-val (async/<! reduced-ch)]
          (recur next-val)
          val)))))

(defn best-output [input settings reducer]
  (->> settings
       u/combos
       (mapv (partial amps input reducer))
       (reduce (fn [best ch]
                 (max best (async/<!! ch)))
               0)))

(comment
  (let [input (first (u/resource 7 (map (comp vec (u/comma-separated (map u/parse-long))))))]
    ;; part one
    (best-output input (set (range 5)) (single-reducer 0))

    ;; part two
    (best-output input (set (range 5 10)) (round-robin-reducer 0))))
