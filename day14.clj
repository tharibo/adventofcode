(defn full-flight-periods [total-duration fly-time rest-time]
  (int (/ total-duration (+ fly-time rest-time))))

(defn final-seconds-of-flight [total-duration fly-time rest-time]
  (mod total-duration (+ fly-time rest-time)))

(defn compute-distance [total-duration speed fly-time rest-time]
  (+ (* speed fly-time (full-flight-periods total-duration fly-time rest-time))
     (* speed (min fly-time (final-seconds-of-flight total-duration fly-time rest-time)))))

(defn compute-all [coll]
  (reduce #(assoc-in %1 [(:name %2) :distance] (compute-distance 2503 (:speed %2) (:fly-time %2) (:rest-time %2))) {} coll))


(def data '({:name "Vixen" :speed 8 :fly-time 8 :rest-time 53}
            {:name "Blitzen" :speed 13 :fly-time 4 :rest-time 49}
            {:name "Rudolph" :speed 20 :fly-time 7 :rest-time 132}
            {:name "Cupid" :speed 12 :fly-time 4 :rest-time 43}
            {:name "Donner" :speed 9 :fly-time 5 :rest-time 38}
            {:name "Dasher" :speed 10 :fly-time 4 :rest-time 37}
            {:name "Comet" :speed 3 :fly-time 37 :rest-time 76}
            {:name "Prancer" :speed 9 :fly-time 12 :rest-time 97}
            {:name "Dancer" :speed 37 :fly-time 1 :rest-time 36}))

