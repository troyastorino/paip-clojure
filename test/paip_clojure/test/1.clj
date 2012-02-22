(ns paip-clojure.test.1
  (:use [paip-clojure.1]
        [clojure.test]))

(deftest name-test
  (is (= "Astorino" (last-name "Arthur Astorino Jr., M.D.")))
  (is (= "d'Anconia" (last-name "Francisco Domingo Carlos Andrews Sebastian d'Anconia")))
  (is (= "Cooper" (last-name "Sheldon Lee Cooper, B.S., M.S., M.A., Ph.D., Sc.D."))))

(deftest math
  (testing "power"
    (is (= 8 (power 2 3)))
    (is (= 81 (power 3 4))))
  (testing "dot-product"
    (is (= 13 (dot-product [3 2] [3 2])))
    (is (= 0 (dot-product [1 0] [0 1])))))

(deftest counting
  (testing "count-atoms"
    (is (= 8 (count-atoms [1 2 {:a 'b} '("hello" 42) [2 5]]))))
  (testing "count-anywhere"
    (is (= 3 (count-anywhere 3 [1 3 [3 6 {:a 3 :b 5}]])))))