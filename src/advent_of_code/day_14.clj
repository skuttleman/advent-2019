(ns advent-of-code.day-14
  (:require [advent-of-code.shared.utils :as u]
            [clojure.string :as string]))

(defn ->resource [s]
  (let [[_ amt res] (re-matches #"(\d+) ([A-Z]+)" (string/trim s))]
    [(keyword (string/lower-case res)) (u/parse-long amt)]))

(defn ->req [s]
  (let [[reqs result] (string/split s #"=>")
        [res amt] (->resource result)]
    [res [amt (map ->resource (string/split reqs #","))]]))

(defn required-ore* [state input building [output-amt [[req input-amt] :as reqs]]]
  (cond
    (empty? reqs) (update-in state [:resources building] (fnil + 0) output-amt)
    (= :ore req) (-> state
                     (update :ore-used (fnil + 0) input-amt)
                     (recur input building [output-amt (rest reqs)]))
    (>= (get-in state [:resources req] 0) input-amt) (-> state (update-in [:resources req] - input-amt)
                                                         (recur input building [output-amt (rest reqs)]))
    :else (-> state
              (required-ore* input req (get input req))
              (recur input building [output-amt reqs]))))

(defn required-ore [input building]
  (:ore-used (required-ore* {} input building (get input building))))

(comment
  (let [input (into {} (u/resource 14 (map ->req)))]
    ;; part one
    (required-ore input :fuel)))
