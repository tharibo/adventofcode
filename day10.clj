(def init-input [3 1 1 3 3 2 2 1 1 3])

(defn toto [coll] (reduce (fn [acc coll] (conj acc (count coll) (first coll))) [] (partition-by identity coll)))

(defn repeat-n-times [n f input]
  (let [result (f input)]
    (if (> n 1)
      (recur (dec n) f result)
      result)))

(count (repeat-n-times 50 toto init-input))
