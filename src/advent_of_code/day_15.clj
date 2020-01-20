(ns advent-of-code.day-15
  (:require [advent-of-code.shared.int-code :as int-code]
            [advent-of-code.shared.utils :as u]
            [clojure.core.async :as async]
            [advent-of-code.shared.directions :as dir]))

(def dir->input
  {:up    1
   :down  2
   :left  3
   :right 4})

(def interpret-status
  {0 :wall
   1 :moved
   2 :oxygen-system})

(defn open-pos [grid pos]
  (sequence (comp (map (partial dir/next-pos pos))
                  (filter (comp #{:visited} grid)))
            [:up :right :down :left]))

(defn next-dirs [{[pos] :trail :keys [grid]}]
  (sequence (comp (map (juxt identity (partial dir/next-pos pos)))
                  (remove (comp grid second))
                  (map first))
            [:up :right :down :left]))

(defn go-back [state]
  (let [next-state (update state :trail rest)]
    (cond-> next-state
      (empty? (rest (:trail next-state))) (assoc :end? true))))

(defn handle-wall [{[pos] :trail :as state} dir]
  (assoc-in state [:grid (dir/next-pos pos dir)] :wall))

(defn handle-moved [{[pos] :trail :as state} dir tag]
  (let [next-pos (dir/next-pos pos dir)]
    (-> state
        (assoc-in [:grid next-pos] tag)
        (update :trail conj next-pos))))

(defn fill-oxygen [{:keys [grid]}]
  (loop [minutes 0
         grid grid
         ox (set (sequence (comp (filter (comp #{:oxygen-system} val)) (map key)) grid))]
    (if-let [next (not-empty (set (mapcat (partial open-pos grid) ox)))]
      (recur (inc minutes)
             (into grid (map #(vector % :oxygen-system)) ox)
             next)
      minutes)))

(defn next-state [state track next-dir status exhaust?]
  (case (or track status)
    :go-back (-> state
                 go-back
                 (update :moves inc))
    :wall (-> state
              (handle-wall next-dir)
              (update :moves inc))
    :moved (-> state
               (handle-moved next-dir :visited)
               (update :moves inc))
    :oxygen-system (cond-> state
                     exhaust? (-> (handle-moved next-dir :oxygen-system)
                                  (update :moves inc))
                     (not exhaust?) (assoc :end? true))))

(defn output [input exhaust?]
  (let [in (async/chan 100 (map dir->input))
        out (async/chan 100 (map interpret-status))]
    (int-code/compute input in out)
    (async/go-loop [{[pos last-pos] :trail :as state} {:trail (list [0 0])
                                                       :grid  {[0 0] :visited}
                                                       :moves 0}]
      (let [[track next-dir] (if-let [next-dir (first (next-dirs state))]
                               [nil next-dir]
                               [:go-back (dir/direction pos last-pos)])]
        (async/>! in next-dir)
        (let [status (async/<! out)
              next-state (next-state state track next-dir status exhaust?)]
          (cond-> next-state
            (not (:end? next-state)) recur))))))

(comment
  (let [input (u/int-code-resource 15)]
    ;; part one
    (count (:trail (async/<!! (output input false))))

    ;; part two
    (fill-oxygen (async/<!! (output input true)))))
