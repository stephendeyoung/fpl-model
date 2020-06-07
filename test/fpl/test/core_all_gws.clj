(ns fpl.test.core-all-gws
  (:require [clojure.test :refer :all]
            [fpl.core-all-gws :refer [calculate-expected-values]]))

(deftest test-calculate-expected-values
  (let [de-bruyne (first (filter #(= (:web_name %) "De Bruyne")
                                 (:elements (calculate-expected-values 36 29 [30 31 32 33 34 35 36]))))]
    (println de-bruyne)
    (is (= 3.431
           (:gw36-expected-points de-bruyne)))))
