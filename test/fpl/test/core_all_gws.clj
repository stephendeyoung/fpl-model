(ns fpl.test.core-all-gws
  (:require [clojure.test :refer :all]
            [fpl.core-all-gws :refer [calculate-expected-values]]
            [clojure.java.io :as io]
            [clojure.data.json :as json]))

(def fpl-data
  (json/read-str (slurp "../resources/fpl_data/gw29.json")
                 :key-fn keyword))

(def player-expected-pts
  (:elements (calculate-expected-values fpl-data 38 29 [30 31 32 33 34 35 36 37 38] false)))

(deftest guaita
  (let [guaita (first (filter #(= (:web_name %) "Guaita")
                              player-expected-pts))]
    (is (= 2.3342221797102107
           (:gw36-expected-points guaita)))))

(deftest doherty
  (let [doherty (first (filter #(= (:web_name %) "Doherty")
                               player-expected-pts))]
    (is (= 3.3825600657105173
           (:gw36-expected-points doherty)))))

(deftest de-bruyne
  (let [de-bruyne (first (filter #(= (:web_name %) "De Bruyne")
                                 player-expected-pts))]
    (is (= 5.028354452697782
           (:gw36-expected-points de-bruyne)))))

(deftest ings
  (let [ings (first (filter #(= (:web_name %) "Ings")
                            player-expected-pts))]
    (is (= 3.9012591715643974
           (:gw36-expected-points ings)))))

(deftest salah
  (let [salah (first (filter #(= (:web_name %) "Salah")
                            player-expected-pts))]
    (is (= 46.35212596404811
           (:expected-points-total salah)))))

(deftest calvert-lewin
  (let [calvert-lewin (first (filter #(= (:web_name %) "Calvert-Lewin")
                             player-expected-pts))]
    (is (= 39.008462809264906
           (:expected-points-total calvert-lewin)))))

(deftest azpilicueta
  (let [azpilicueta (first (filter #(= (:web_name %) "Azpilicueta")
                                     player-expected-pts))]
    (is (= 30.836193822803878
           (:expected-points-total azpilicueta)))))

(deftest schmeichel
  (let [schmeichel (first (filter #(= (:web_name %) "Schmeichel")
                                   player-expected-pts))]
    (is (= 21.7016306495753
           (:expected-points-total schmeichel)))))

(deftest mahrez
  (let [mahrez (first (filter #(= (:web_name %) "Mahrez")
                                  player-expected-pts))]
    (is (= 3.908816283732894
           (:expected_points_per_90 mahrez)))))

(deftest abraham
  (let [abraham (first (filter #(= (:web_name %) "Abraham")
                              player-expected-pts))]
    (is (= 3.3868048908366215
           (:expected_points_per_90 abraham)))))

(deftest lundstram
  (let [lundstram (first (filter #(= (:web_name %) "Lundstram")
                               player-expected-pts))]
    (is (= 2.6760184648207987
           (:expected_points_per_90 lundstram)))))

(deftest patricio
  (let [patricio (first (filter #(= (:web_name %) "Patrício")
                                 player-expected-pts))]
    (is (= 2.2907519423401093
           (:expected_points_per_90 patricio)))))

(def player-expected-pts-double-gw
  (:elements (calculate-expected-values fpl-data 30 29 [30] false)))

(deftest mcburnie-double-gw
  (let [mcburnie (first (filter #(= (:web_name %) "McBurnie")
                                player-expected-pts-double-gw))]
    (is (= 7.788250098896126
           (:expected-points-total mcburnie)))))

(def player-expected-pts-blank
  (:elements (calculate-expected-values fpl-data 29 27 [28 29] false)))

(deftest mcburnie-blank
  (let [mcburnie (first (filter #(= (:web_name %) "McBurnie")
                                player-expected-pts-blank))]
    (is (= 0
           (:gw28-expected-points mcburnie)))
    (is (= 4.057807870785459
           (:expected-points-total mcburnie)))))
