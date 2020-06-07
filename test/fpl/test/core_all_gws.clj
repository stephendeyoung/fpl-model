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
    (is (= 0.3445984732516715
           (:gw36-expected-points guaita)))))

(deftest doherty
  (let [doherty (first (filter #(= (:web_name %) "Doherty")
                               player-expected-pts))]
    (is (= 1.444517466846757
           (:gw36-expected-points doherty)))))

(deftest de-bruyne
  (let [de-bruyne (first (filter #(= (:web_name %) "De Bruyne")
                                 player-expected-pts))]
    (is (= 3.4341467303369257
           (:gw36-expected-points de-bruyne)))))

(deftest ings
  (let [ings (first (filter #(= (:web_name %) "Ings")
                            player-expected-pts))]
    (is (= 2.1537609470510515
           (:gw36-expected-points ings)))))

(deftest salah
  (let [salah (first (filter #(= (:web_name %) "Salah")
                            player-expected-pts))]
    (is (= 28.65329180422953
           (:expected-points-total salah)))))

(deftest calvert-lewin
  (let [calvert-lewin (first (filter #(= (:web_name %) "Calvert-Lewin")
                             player-expected-pts))]
    (is (= 18.983790967584184
           (:expected-points-total calvert-lewin)))))

(deftest azpilicueta
  (let [azpilicueta (first (filter #(= (:web_name %) "Azpilicueta")
                                     player-expected-pts))]
    (is (= 12.302203978916982
           (:expected-points-total azpilicueta)))))

(deftest schmeichel
  (let [schmeichel (first (filter #(= (:web_name %) "Schmeichel")
                                   player-expected-pts))]
    (is (= 3.7075341487074276
           (:expected-points-total schmeichel)))))

(deftest mahrez
  (let [mahrez (first (filter #(= (:web_name %) "Mahrez")
                                  player-expected-pts))]
    (is (= 2.5371977515688453
           (:expected_points_per_90 mahrez)))))

(deftest abraham
  (let [abraham (first (filter #(= (:web_name %) "Abraham")
                              player-expected-pts))]
    (is (= 2.45416221
           (:expected_points_per_90 abraham)))))

(deftest lundstram
  (let [lundstram (first (filter #(= (:web_name %) "Lundstram")
                               player-expected-pts))]
    (is (= 1.215447328820798
           (:expected_points_per_90 lundstram)))))

(deftest patricio
  (let [patricio (first (filter #(= (:web_name %) "Patrício")
                                 player-expected-pts))]
    (is (= 0.29075194234010915
           (:expected_points_per_90 patricio)))))
