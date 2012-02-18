(ns paip-clojure.1
  (:require [clojure.string :as str]))

;; 1.1
(defn suffix? [x]
  (let [suffixes #{"Sr" "Senior" "Jr" "Junior" "PhD" "MD" "DO"
                   "DC" "MBA" "JD" "Esq" "Esquire" "DD"}
        regularize #(str/lower-case (apply str (str/split %1 #"[.]")))]
    ((complement not-any?) #(apply = (map regularize [x %1])) suffixes)))

(defn last-name [name]
  (let [str->seq #(filter (complement empty?) (str/split %1 #"[ ,]"))]
    (if (string? name)
      (last-name (str->seq name))
      (-> (filter (complement suffix?) (reverse name))
          first str/lower-case str/capitalize))))

;; 1.2
(defn power [x n]
  {:pre [(integer? n) (>= n 0)]}
  (cond
   (= 0 n) 1
   (even? n) (power (* x x) (/ n 2))
   :else (* x (power x (dec n)))))

;; 1.3
(defn count-atoms [x]
  (reduce #(+ %1 (if (coll? %2) (count-atoms %2) 1)) 0 x))

;; 1.4
(defn count-anywhere [x coll]
  (reduce #(+ %1 (if (coll? %2)
                   (count-anywhere x %2)
                   (if (= x %2) 1 0)))
          0 coll))

;; 1.5
(defn dot-product [u v]
  (reduce + (map * u v)))
