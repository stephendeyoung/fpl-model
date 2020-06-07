(ns fpl.test.scoring
  (:require [clojure.test :refer :all]
            [fpl.scoring :refer [rules]]))

(deftest gk-scoring
  (let [gk-scoring-fn (:gk-scores rules)]
    (testing "nil")
    (testing "empty list"
      (let [team-xg-concededs []]))

    (testing "one xg conceded in list")
    (testing "many xg conceded in list")))