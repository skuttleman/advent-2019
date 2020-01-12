(ns advent-of-code.day-5
  (:require [advent-of-code.shared.int-code :as int-code]
            [advent-of-code.utils :as u]
            [clojure.core.async :as async]))

(defn diagnostic-code [input in-val]
  (let [in (async/chan 100)
        out (async/chan 100)]
    (int-code/compute input in out)
    (async/put! in in-val)
    (async/<!! (async/go-loop [val (async/<! out)]
                 (if-let [next-val (async/<! out)]
                   (recur next-val)
                   val)))))

(comment
  (let [input (first (u/resource 5 (map (comp vec (u/comma-separated (map u/parse-long))))))]
    ;; part one
    (diagnostic-code input 1)

    ;; part two
    (diagnostic-code input 5)))
