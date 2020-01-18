(ns advent-of-code.day-9
  (:require [advent-of-code.shared.int-code :as int-code]
            [advent-of-code.shared.utils :as u]
            [clojure.core.async :as async]))

(defn output [input in-val]
  (let [in (async/chan 100)
        out (async/chan 100)]
    (int-code/compute input in out)
    (async/put! in in-val)
    (async/go-loop [coll []]
      (if-let [val (async/<! out)]
        (recur (conj coll val))
        coll))))

(comment
  (let [input (u/int-code-resource 9)]
    ;; part one
    (first (async/<!! (output input 1)))

    ;; part two
    (first (async/<!! (output input 2)))))
