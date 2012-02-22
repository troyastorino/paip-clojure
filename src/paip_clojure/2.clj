(ns paip-clojure.2
  (:require [clojure.string :as str]))

(def simple-grammar
  {:sentence [[:noun-phrase :verb-phrase]]
   :noun-phrase [[:Article :Noun]]
   :verb-phrase [[:Verb :noun-phrase]]
   :Article ["the" "a"]
   :Noun ["man" "ball" "woman" "table"]
   :Verb ["hit" "took" "saw" "liked"]})

(defn generate [grammar phrase]
  (cond
   (vector? phrase) (apply concat (map (partial generate grammar) phrase))
   (grammar phrase) (generate grammar (rand-nth (grammar phrase)))
   :else (list phrase)))

;; 2.1 / 2.2
(defn generate [grammar phrase]
  (let [gen (partial generate grammar)]
    (cond
     (vector? phrase) (apply concat (map gen phrase))
     (keyword? phrase) (gen (rand-nth (grammar phrase)))
     :else (list phrase))))

(def bigger-grammar
  {:sentence [[:noun-phrase :verb-phrase]]
   :noun-phrase [[:Article :Adj* :Noun :PP*] [:Name] [:Pronoun]]
   :verb-phrase [[:Verb :noun-phrase :PP*]]
   :PP* [[] [:PP :PP*]]
   :Adj* [[] [:Adj :Adj*]]
   :PP [[:Prep :noun-phrase]]
   :Prep ["to" "in" "by" "with" "on"]
   :Adj ["big" "little" "blue" "green" "adiabatic"]
   :Article ["the" "a"]
   :Name ["Pat" "Kim" "Lee" "Terry" "Robin"]
   :Noun ["man" "ball" "woman" "table"]
   :Verb ["hit" "took" "saw" "liked"]
   :Pronoun ["he" "she" "it" "these" "those" "that"]})

(defn generate-tree [grammar phrase]
  (let [gen-tree (partial generate-tree grammar)]
    (cond
     (vector? phrase) (map gen-tree phrase)
     (keyword? phrase) (cons phrase (gen-tree (rand-nth (grammar phrase))))
     :else (list phrase))))

(defn combine-all [x y]
  (for [j y i x]
    [i j]))

(defn generate-all [grammar phrase]
  (let [gen-all (partial generate-all grammar)]
    (cond
     (vector? phrase) (combine-all (gen-all (first phrase))
                                   (apply concat (map gen-all (rest phrase))))
     (keyword? phrase) (apply concat (map gen-all (grammar phrase)))
     :else (list phrase))))

;; 2.3
(def simple-french-grammar
  {:sentence [[:noun-phrase :verb-phrase]]
   :noun-phrase [[:Masculine-Article :Masculine-Noun]
                 [:Feminine-Article :Feminine-Noun]]
   :verb-phrase [[:Verb :noun-phrase]]
   :Masculine-Article ["le" "un"]
   :Feminine-Article ["la" "une"]
   :Masculine-Noun ["homme" "ballon"]
   :Feminine-Noun ["femme" "table"]
   :Verb ["frappe" "prend" "vu" "aime"]})

;; 2.4
(defn cross-product [f x y]
  (for [j y i x]
    (f i j)))

(defn combine-all [x y]
  (cross-product list x y))