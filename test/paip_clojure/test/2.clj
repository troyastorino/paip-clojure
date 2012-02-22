(ns paip-clojure.test.2
  (:use [paip-clojure.2]
        [clojure.test]))

(deftest generating
  (testing "generate"
    (is (reduce #(and %1 %2) (map string? (generate simple-grammar :sentence))))
    (is (reduce #(and %1 %2) (map string? (generate bigger-grammar :sentence)))))
  (testing "combine-all"
    (is (= '([1 3] [2 3] [1 4] [2 4]) (combine-all [1 2] [3 4]))))
  (testing "generate-all"
    (is (= 256 (count (generate-all simple-grammar :sentence)))))
  (testing "cross-product"
    (is (= [11 12 13 21 22 23 31 32 33]
           (cross-product + [1 2 3] [10 20 30])))))

