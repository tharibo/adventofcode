(ns adventofcode.day19)
(use 'clojure.java.io)
(def replacement-strings (let [r (reader "C:/users/tharibo/Documents/dev/adventofcode/day19.input.txt")]
                        (line-seq r)))

(def input-str "CRnCaSiRnBSiRnFArTiBPTiTiBFArPBCaSiThSiRnTiBPBPMgArCaSiRnTiMgArCaSiThCaSiRnFArRnSiRnFArTiTiBFArCaCaSiRnSiThCaCaSiRnMgArFYSiRnFYCaFArSiThCaSiThPBPTiMgArCaPRnSiAlArPBCaCaSiRnFYSiThCaRnFArArCaCaSiRnPBSiRnFArMgYCaCaCaCaSiThCaCaSiAlArCaCaSiRnPBSiAlArBCaCaCaCaSiThCaPBSiThPBPBCaSiRnFYFArSiThCaSiRnFArBCaCaSiRnFYFArSiThCaPBSiThCaSiRnPMgArRnFArPTiBCaPRnFArCaCaCaCaSiRnCaCaSiRnFYFArFArBCaSiThFArThSiThSiRnTiRnPMgArFArCaSiThCaPBCaSiRnBFArCaCaPRnCaCaPMgArSiRnFYFArCaSiThRnPBPMgAr")

(def replacement-re #"([A-Za-z]+) => ([A-Za-z]+)")
(defn create-replacements [s]
  (map #(assoc {}
               :find (get % 1)
               :replace (get % 2))
       (map #(re-find replacement-re %) s)))
(def replacements (create-replacements replacement-strings))

(def test-replacement-strings '(
                                "H => HO"
                                "H => OH"
                                "O => HH"
                                "e => H"
                                "e => O"
))
(def test-replacements (create-replacements test-replacement-strings))

(defn all-indexes-of
  ([s value] (all-indexes-of s value nil))
  ([s value coll]
   (let [i (.indexOf s value)]
     ;(prn s value coll)
     (if (= -1 i) coll
         (recur (subs s (inc i))
                value
                (if (nil? coll) (vector i) (conj coll (+ 1 (last coll) i))))))))

(defn replace-at [s x y start-index]
  (let [part1 (subs s 0 start-index)
        part2 (subs s start-index)
        replaced (clojure.string/replace-first part2 x y)]
    ;(prn part1)
    ;(prn part2)
    ;(prn replaced)
    (str part1 replaced)))

(defn all-molecules [s replacement]
  (let [indexes (all-indexes-of s (:find replacement))]
    (map (partial replace-at s (:find replacement) (:replace replacement)) indexes)))

(defn all-possibilities [s replacements]
  (reduce #(concat %1 (all-molecules s %2)) '() replacements))

(def distinct-possibilities (count (distinct (all-possibilities input-str replacements))))

(defn all-simplifications [s replacement]
  (let [indexes (all-indexes-of s (:replace replacement))]
    (map (partial replace-at s (:replace replacement) (:find replacement)) indexes)))

(defn all-possible-simplifications [s replacements]
  (reduce #(concat %1 (all-simplifications s %2)) '() replacements))

(defn simplify-step [coll replacements]
  "Takes each string in coll and find all their simplifications. Those simplifications are then all returned"
  (reduce #(concat %1 (all-possible-simplifications %2 replacements)) [] coll))

(defn how-many-steps-from-electron?
  ([s replacements] (how-many-steps-from-electron? [s] replacements 1))
  ([coll replacements steps]
   (let [step-results (distinct (simplify-step coll replacements))
         found-electron? (some #{"e"} step-results)]
     (prn (first step-results) (count step-results))
     (if found-electron? steps
         (recur step-results replacements (inc steps))))))

; un chemin de simplifications peut amener à une impasse
; une fois qu'un chemin a été parcouru, on peut stocker le nombre d'étapes.

(defn step
  ([s replacements] (step s replacements 0 {}))
  ([s replacements steps history]
   (if (= s "e") steps
       (let [coll (distinct (all-possible-simplifications s replacements))
             filtered (filter (complement #(some #{%} history)) coll)]
         (reduce #(let [nb-steps (step %2 replacements (inc steps) (concat history filtered))]
                    (when (= steps 0) (prn "."))
                    (cond (= 0 nb-steps) %1
                          (= 0 %1) nb-steps
                          :default (min nb-steps %1))) 0 filtered)))))
