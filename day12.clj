(ns adventofcode.day12
  (:require [clojure.data.json :as json]))

(use 'clojure.java.io)

(def input-json (let [r (reader "D:/users/thomas.ribo/GitRepos/adventofcode/day12.input.txt")]
  (json/read r)))

(defn flatten-json [node]
  (lazy-seq
   (cond (map? node) (mapcat flatten-json (vals node))
         (or (seq? node) (vector? node)) (mapcat flatten-json node)
         (number? node) (list node)
         :default nil)))

(defn flatten-json2 [node]
  (lazy-seq
   (cond (map? node) (if (some #{"red"} (vals node)) (list 0) (mapcat flatten-json2 (vals node)))
         (or (seq? node) (vector? node)) (mapcat flatten-json2 node)
         (number? node) (list node)
         :default nil)))

         
(reduce + (flatten-json input-json))
(reduce + (flatten-json2 input-json))
