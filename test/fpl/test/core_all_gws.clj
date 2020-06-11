(ns fpl.test.core-all-gws
  (:require [clojure.test :refer :all]
            [fpl.core-all-gws :refer [calculate-expected-values]]
            [clojure.java.io :as io]
            [clojure.data.json :as json]))

(def fpl-data
  (json/read-str (slurp "../resources/fpl_data/gw29.json")
                 :key-fn keyword))

(def player-expected-pts
  (:elements (calculate-expected-values fpl-data 38 29 [30 31 32 33 34 35 36 37 38])))

(deftest guaita
  (let [guaita (first (filter #(= (:web_name %) "Guaita")
                              player-expected-pts))]
    (is (= 2.3192907794259003
           (:gw36-expected-points guaita)))))

(deftest doherty
  (let [doherty (first (filter #(= (:web_name %) "Doherty")
                               player-expected-pts))]
    (is (= 3.3506480703123787
           (:gw36-expected-points doherty)))))

(deftest de-bruyne
  (let [de-bruyne (first (filter #(= (:web_name %) "De Bruyne")
                                 player-expected-pts))]
    (is (= 4.866228142289397
           (:gw36-expected-points de-bruyne)))))

(deftest ings
  (let [ings (first (filter #(= (:web_name %) "Ings")
                            player-expected-pts))]
    (is (= 3.914864872456932
           (:gw36-expected-points ings)))))

(deftest salah
  (let [salah (first (filter #(= (:web_name %) "Salah")
                            player-expected-pts))]
    (is (= 46.07068502973058
           (:expected-points-total salah)))))

(deftest calvert-lewin
  (let [calvert-lewin (first (filter #(= (:web_name %) "Calvert-Lewin")
                             player-expected-pts))]
    (is (= 34.405747003643775
           (:expected-points-total calvert-lewin)))))

(deftest azpilicueta
  (let [azpilicueta (first (filter #(= (:web_name %) "Azpilicueta")
                                     player-expected-pts))]
    (is (= 27.94108163715241
           (:expected-points-total azpilicueta)))))

(deftest schmeichel
  (let [schmeichel (first (filter #(= (:web_name %) "Schmeichel")
                                   player-expected-pts))]
    (is (= 19.707534148707428
           (:expected-points-total schmeichel)))))

(deftest mahrez
  (let [mahrez (first (filter #(= (:web_name %) "Mahrez")
                                  player-expected-pts))]
    (is (= 3.712100200548289
           (:expected_points_per_90 mahrez)))))

(deftest abraham
  (let [abraham (first (filter #(= (:web_name %) "Abraham")
                              player-expected-pts))]
    (is (= 3.578479631626891
           (:expected_points_per_90 abraham)))))

(deftest lundstram
  (let [lundstram (first (filter #(= (:web_name %) "Lundstram")
                               player-expected-pts))]
    (is (= 2.727405483809079
           (:expected_points_per_90 lundstram)))))

(deftest patricio
  (let [patricio (first (filter #(= (:web_name %) "Patrício")
                                 player-expected-pts))]
    (is (= 2.2907519423401093
           (:expected_points_per_90 patricio)))))

(def player-expected-pts-double-gw
  (:elements (calculate-expected-values fpl-data 30 29 [30])))

(deftest mcburnie-double-gw
  (let [mcburnie (first (filter #(= (:web_name %) "McBurnie")
                                player-expected-pts-double-gw))]
    (is (= 7.620434687279806
           (:expected-points-total mcburnie)))))

(def player-expected-pts-blank
  (:elements (calculate-expected-values fpl-data 29 27 [28 29])))

(deftest mcburnie-blank
  (let [mcburnie (first (filter #(= (:web_name %) "McBurnie")
                                player-expected-pts-blank))]
    (is (= 0
           (:gw28-expected-points mcburnie)))
    (is (= 3.9739001649773
           (:expected-points-total mcburnie)))))
