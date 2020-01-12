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

(defmulti get-arg (comp first list))
(defmethod get-arg 0 [_ input arg]
  (get input arg))

(defmethod get-arg 1 [_ _ arg]
  arg)

(defn math-step [input pos f [arg-1 arg-mode-1 arg-2 arg-mode-2 arg-3]]
  [(assoc input arg-3 (f (get-arg arg-mode-1 input arg-1)
                          (get-arg arg-mode-2 input arg-2)))
   (+ 4 pos)])

(defn read-step [input pos std-in [arg]]
  (async/go
    [(assoc input arg (async/<! std-in))
     (+ 2 pos)]))

(defn write-step [input pos std-out [arg arg-mode]]
  (async/go
    (async/>! std-out (get-arg arg-mode input arg))
    [input (+ 2 pos)]))

(defn jump-step [input pos pred [arg-1 arg-mode-1 arg-2 arg-mode-2]]
  (if (pred (get-arg arg-mode-1 input arg-1))
    [input (get-arg arg-mode-2 input arg-2)]
    [input (+ 3 pos)]))

(defn compare-step [input pos compare [arg-1 arg-mode-1 arg-2 arg-mode-2 arg-3]]
  [(assoc input arg-3 (if (compare (get-arg arg-mode-1 input arg-1)
                                    (get-arg arg-mode-2 input arg-2))
                         1
                         0))
   (+ 4 pos)])

(defn compute
  ([input]
   (async/<!! (compute input nil nil)))
  ([input std-in std-out]
   (async/go-loop [[input pos] [input 0]]
     (let [[op & args] (drop pos input)
           [op & modes] (->op op)
           args+modes (interleave args modes)]
       (condp = op
         1 (recur (math-step input pos + args+modes))
         2 (recur (math-step input pos * args+modes))
         3 (recur (async/<! (read-step input pos std-in args+modes)))
         4 (recur (async/<! (write-step input pos std-out args+modes)))
         5 (recur (jump-step input pos (complement zero?) args+modes))
         6 (recur (jump-step input pos zero? args+modes))
         7 (recur (compare-step input pos < args+modes))
         8 (recur (compare-step input pos = args+modes))
         99 (do (run! async/close! [std-in std-out])
                input))))))
