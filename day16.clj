(ns adventofcode.day16)
(use 'clojure.java.io)
(def txt-strings (let [r (reader "C:/users/tharibo/Documents/dev/adventofcode/day16.input.txt")]
                        (line-seq r)))
(def sue-index-re #"Sue (\d+):")
(def property-re #"([a-z]+): (\d+)")
;(re-seq regexp input-txt)
;(["perfumes: 6" "perfumes" "6"] ["goldfish: 3" "goldfish" "3"] ["vizslas: 7" "vizslas" "7"])
(defn sue
  ([s]
   (let [index (get (re-find sue-index-re s) 1)
         properties (re-seq property-re s)]
     (sue index properties)))
  ([index matches]
   (reduce #(let [k (keyword (get %2 1))
                  v (get %2 2)]
              (assoc %1 k (Integer/parseInt v)))
           {:sue (Integer/parseInt index)}
           matches)))

(def sues (map sue txt-strings))

(def the-right-sue {:sue 999
                    :children 3
                    :cats 7
                    :samoyeds 2
                    :pomeranians 3
                    :akitas 0
                    :vizslas 0
                    :goldfish 5
                    :trees 3
                    :cars 2
                    :perfumes 1})

(defn could-be-sue? [the-right-sue sue]
  (if (reduce (fn [_ [k v]]
                (if (or (= k :sue) (= (k the-right-sue) v))
                  true
                  (reduced false)))
          true
          sue)
    (:sue sue)
    false))
  
(defn could-be-sue2? [the-right-sue sue]
  (if (reduce (fn [_ [k v]]
                (if (= k :sue) true
                    (cond (some #{k} [:cats :trees]) (if (< (k the-right-sue) v) true (reduced false))
                          (some #{k} [:pomeranians :goldfish]) (if (> (k the-right-sue) v) true (reduced false))
                          :default (if (= (k the-right-sue) v) true (reduced false)))))
              true
              sue)
    (:sue sue)
    false))
  
(some #(when (number? %) %) (map (partial could-be-sue? the-right-sue) sues))
(some #(when (number? %) %) (map (partial could-be-sue2? the-right-sue) sues))
