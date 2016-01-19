(ns adventofcode.day6)
(use 'clojure.java.io)

(def instructions-txt (slurp "D:/users/thomas.ribo/GitRepos/adventofcode/day6.input.txt"))


(def regexp #"(turn on|turn off|toggle)\s+(\d+,\d+)\s+through\s+(\d+,\d+)")
(def instructions (re-seq regexp instructions-txt))
(def grid (apply vector (repeat 1000 (apply vector (repeat 1000 0)))))

(defn set-on [old-value] true)
(defn set-off [old-value] false)
(defn set-default [old-value] :default)
(defn set-toggled [old-value] (not old-value))

(defn set-on2 [old-value] (inc old-value))
(defn set-off2 [old-value] (max 0 (dec old-value)))
(defn set-default2 [old-value] :default)
(defn set-toggled2 [old-value] (+ 2 old-value))

(defn switch [grid [from-x from-y] [to-x to-y] on-or-off]
  (def coords (for [x (range from-x (inc to-x))
                    y (range from-y (inc to-y))]
                [x y]))
  (reduce (fn [grid [x y]]
            (assoc-in grid [x y] (on-or-off (get-in grid [x y]))))
          grid coords))

(switch grid [0 0] [3 3] set-toggled)

(defn translate [s]
  (cond (= "turn on" s) set-on
        (= "turn off" s) set-off
        (= "toggle" s) set-toggled
        :default  set-default))

(defn translate2 [s]
  (cond (= "turn on" s) set-on2
        (= "turn off" s) set-off2
        (= "toggle" s) set-toggled2
        :default  set-default2))

(defn str-to-coord [s]
  (map #(Integer/parseInt %) (clojure.string/split s #",")))

;(def instructions-obj (drop 1 (reductions #(hash-map :action (translate (get %2 1))
;                                                     :from (str-to-coord (get %2 2))
;                                                     :to (str-to-coord (get %2 3)))
;                                          {}
;                                          instructions)))
;(def final-grid (reduce (fn [grid instruction] (switch grid (:from instruction) (:to instruction) (:action instruction)))
;        grid
;        instructions-obj))
;(count (filter identity (flatten final-grid)))

(def instructions-obj2 (drop 1 (reductions #(hash-map :action (translate2 (get %2 1))
                                                     :from (str-to-coord (get %2 2))
                                                     :to (str-to-coord (get %2 3)))
                                          {}
                                          instructions)))
(def final-grid2 (reduce (fn [grid instruction] (switch grid (:from instruction) (:to instruction) (:action instruction)))
        grid
        instructions-obj2))
(apply + (flatten final-grid2))
