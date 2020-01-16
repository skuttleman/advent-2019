(ns advent-of-code.shared.int-code
  (:require [advent-of-code.utils :as u]
            [clojure.core.async :as async]))

(defn ->op [n]
  (let [str-n (str n)
        len (count str-n)
        off-by-2 (- len 2)
        op (if (pos? off-by-2)
             (u/parse-long (subs str-n off-by-2))
             n)
        codes (when (pos? off-by-2)
                (map u/parse-long (reverse (subs str-n 0 off-by-2))))]
    (concat [op] codes (repeat 0))))

(defmulti write-arg (comp peek vector))

(defmethod write-arg :default [_ arg _]
  arg)

(defmethod write-arg 2 [{:keys [rel-base]} arg _]
  (+ rel-base arg))

(defmulti get-arg (comp peek vector))

(defmethod get-arg 0 [{:keys [input]} arg _]
  (get input arg 0))

(defmethod get-arg 1 [_ arg _]
  arg)

(defmethod get-arg 2 [{:keys [input rel-base]} arg _]
  (get input (+ rel-base arg) 0))

(defn math-step [state f [arg-1 arg-mode-1 arg-2 arg-mode-2 arg-3 arg-mode-3]]
  (-> state
      (update :input
              assoc
              (write-arg state arg-3 arg-mode-3)
              (f (get-arg state arg-1 arg-mode-1)
                 (get-arg state arg-2 arg-mode-2)))
      (update :pos + 4)))

(defn read-step [state std-in [arg arg-mode]]
  (async/go
    (-> state
        (update :input assoc (write-arg state arg arg-mode) (async/<! std-in))
        (update :pos + 2))))

(defn write-step [state std-out [arg arg-mode]]
  (async/go
    (async/>! std-out (get-arg state arg arg-mode))
    (update state :pos + 2)))

(defn jump-step [state pred [arg-1 arg-mode-1 arg-2 arg-mode-2]]
  (if (pred (get-arg state arg-1 arg-mode-1))
    (assoc state :pos (get-arg state arg-2 arg-mode-2))
    (update state :pos + 3)))

(defn compare-step [state compare [arg-1 arg-mode-1 arg-2 arg-mode-2 arg-3 arg-mode-3]]
  (-> state
      (update :input
              assoc
              (write-arg state arg-3 arg-mode-3)
              (if (compare (get-arg state arg-1 arg-mode-1)
                                              (get-arg state arg-2 arg-mode-2))
                                   1
                                   0))
      (update :pos + 4)))

(defn relative-step [state [arg-1 arg-mode-1]]
  (-> state
      (update :rel-base + (get-arg state arg-1 arg-mode-1))
      (update :pos + 2)))

(defn compute
  ([input]
   (async/<!! (compute input nil nil)))
  ([input std-in std-out]
   (async/go-loop [{:keys [input pos] :as state} {:input    (zipmap (range) input)
                                                  :pos      0
                                                  :rel-base 0}]
     (let [[op* & args] (map #(get input % 0) (range pos (+ 5 pos)))
           [op & modes] (->op op*)
           args+modes (interleave args modes)]
       ;(println (with-out-str (pp/pprint [pos state])))
       (condp = op
         1 (recur (math-step state + args+modes))
         2 (recur (math-step state * args+modes))
         3 (recur (async/<! (read-step state std-in args+modes)))
         4 (recur (async/<! (write-step state std-out args+modes)))
         5 (recur (jump-step state (complement zero?) args+modes))
         6 (recur (jump-step state zero? args+modes))
         7 (recur (compare-step state < args+modes))
         8 (recur (compare-step state = args+modes))
         9 (recur (relative-step state args+modes))
         99 (do (run! async/close! (filter some? [std-in std-out]))
                (->> input
                     keys
                     (reduce max -1)
                     inc
                     range
                     (mapv #(get input % 0)))))))))
