(ns adventofcode.day20)

(defn positive-numbers 
	([] (positive-numbers 1))
	([n] (lazy-seq (cons n (positive-numbers (inc n))))))

(defn elf-for-house [elf house]
  (if (= 0 (mod house elf)) (* 10 elf)
      0))

(defn compute-presents [house]
  (reduce #(+ %1 (elf-for-house %2 house)) 0 (range 1 (inc house))))

(defn lazy-house-presents
  ([] (lazy-house-presents 1))
  ([house] (lazy-seq (cons (compute-presents house) (lazy-house-presents (inc house))))))

'(take-while (partial >= 36000000) (lazy-house-presents))

(defn count-while
  ([pred coll] (count-while pred coll 0))
  ([pred coll acc]
   (let [s (seq coll)]
     (if (and s (pred (first s)))
       (recur pred (rest s) (inc acc))
       acc))))
