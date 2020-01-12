(ns advent-of-code.day-4)

(defn no-decs? [n]
  (let [xs (seq (str n))]
    (= xs (sort xs))))

(defn has-dup? [n]
  (let [xs (seq (str n))]
    (> (count xs) (count (distinct xs)))))

(defn has-double? [n]
  (-> (str n)
      frequencies
      vals
      set
      (contains? 2)))

(comment
  (let [input (range 168630 718098)]
    ;; part one
    (->> input
         (filter no-decs?)
         (filter has-dup?)
         count)

    ;; part two
    (->> input
         (filter no-decs?)
         (filter has-double?)
         count)))
