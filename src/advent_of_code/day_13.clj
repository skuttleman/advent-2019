(ns advent-of-code.day-13
  (:require [advent-of-code.shared.int-code :as int-code]
            [advent-of-code.shared.utils :as u]
            [clojure.core.async :as async]))

(defn output [input]
  (let [in (async/chan 100)
        out (async/chan 100)]
    (int-code/compute input in out)
    (async/go-loop [coll []]
      (if-let [val (async/<! out)]
        (recur (conj coll val))
        coll))))

(def paddle?
  (partial = 3))

(def ball?
  (partial = 4))

(defn score? [x y]
  (and (= -1 x) (zero? y)))

(defn play [input]
  (let [in (async/chan 100)
        out (async/chan 100)]
    (int-code/compute input in out)
    (async/go-loop [{[ball-x] :ball [paddle-x] :paddle :as state} {}]
      (if-some [x (async/<! out)]
        (let [y (async/<! out)
              v (async/<! out)]
          (cond
            (score? x y) (recur (assoc state :score v))
            (paddle? v) (recur (assoc state :paddle [x y]))
            (ball? v) (let [next-dir (when ball-x
                                       (compare x ball-x))
                            next-state (assoc state :ball [x y next-dir])]
                        (async/>! in (if (and next-dir paddle-x)
                                       (compare x paddle-x)
                                       0))
                        (recur next-state))
            :else (recur state)))
        (:score state)))))

(comment
  (compare 3 nil)
  (let [input (u/int-code-resource 13)]
    ;; part one
    (->> (output input)
         async/<!!
         (partition 3)
         (map (comp second rest))
         (filter #{2})
         count)

    ;; part two
    (-> input
        (assoc 0 2)
        play
        async/<!!)))
