(ns adventofcode.day15)

(def test-ingredients
  [{:name "Butterscotch", :capacity -1, :durability -2, :flavor 6, :texture 3, :calories 8}
   {:name "Cinnamon", :capacity 2, :durability 3, :flavor -2, :texture -1, :calories 3}])

(def ingredients
  [{:name "Sugar", :capacity 3, :durability 0, :flavor 0, :texture -3, :calories 2}
   {:name "Sprinkles", :capacity -3, :durability 3, :flavor 0, :texture 0, :calories 9}
   {:name "Candy", :capacity -1, :durability 0, :flavor 4, :texture 0, :calories 1}
   {:name "Chocolate", :capacity 0, :durability 0, :flavor -2, :texture 2, :calories 8}])

(defn ingredient [name capacity durability flavor texture calories]
  {:name name
   :capacity capacity
   :durability durability
   :flavor flavor
   :texture texture
   :calories calories})

(def total-score-properties [:capacity :durability :flavor :texture])

(defn property-score [cardinalities values]
  (max 0
       (reduce + (map (partial apply *) (map vector cardinalities values)))))
(defn property-values [ingredients property]
  (map property ingredients))

(defn total-score [cardinalities ingredients]
  (let [properties-values (map (partial property-values ingredients) total-score-properties)
        properties-scores (map (partial property-score cardinalities) properties-values)]
    (apply * properties-scores)))

(defn calories [cardinalities ingredients]
  (property-score cardinalities (property-values ingredients :calories)))

(defn next-cardinalities
  ([coll] (next-cardinalities coll (apply + coll)))
  ([[x y & restcoll] nb-resources]
   (if (nil? y) nil
       (if (= y 1)
         (cons 1 (next-cardinalities (cons y restcoll) (dec nb-resources)))
         (let [right (cons (dec y) restcoll)
               total-right (apply + right)
               value (- nb-resources total-right)]
           (cons value right))))))


(defn all-cardinalities [size nb-resources]
  (loop [coll (concat (take (dec size) (repeat 1)) [(- nb-resources (dec size))])
         cardinalities []]
    (if (or (nil? coll)
            (< (count coll) size)) cardinalities
        (recur (next-cardinalities coll) (conj cardinalities coll)))))
  
(defn best-combination [ingredients]
  (let [cardinalities (all-cardinalities (count ingredients) 100)]
    (reduce #(let [new-score (total-score %2 ingredients)]
               (if (> new-score %1) new-score
                   %1))
            0 cardinalities)))

(defn best-combination-500 [ingredients]
  (let [cardinalities (all-cardinalities (count ingredients) 100)]
    (reduce #(let [new-score (total-score %2 ingredients)
                   calorie-count (calories %2 ingredients)]
               (if (and (= calorie-count 500)
                        (> new-score %1))
                 new-score
                 %1))
            0 cardinalities)))
