(ns fpl.test.core-all-gws
  (:require [clojure.test :refer :all]
            [fpl.core-all-gws :refer [calculate-expected-values]]
            [clojure.java.io :as io]
            [clojure.data.json :as json]
            [clojure.edn :as edn]))

(def fpl-data
  (json/read-str (slurp "../resources/fpl_data/gw29.json")
                 :key-fn keyword))

(def fixtures
  (edn/read-string (slurp "../resources/fixtures-test.edn")))

(def player-expected-pts
  (:elements (calculate-expected-values fpl-data fixtures 38 29 [30 31 32 33 34 35 36 37 38] :ignore-appearances
                                        false :test? true)))

(deftest guaita
  (let [guaita (first (filter #(= (:web_name %) "Guaita")
                              player-expected-pts))]
    (is (= 2.9770793225673535
           (:gw36-expected-points guaita)))))

(deftest doherty
  (let [doherty (first (filter #(= (:web_name %) "Doherty")
                               player-expected-pts))]
    (is (= 3.570451506211561
           (:gw36-expected-points doherty)))))

(deftest de-bruyne
  (let [de-bruyne (first (filter #(= (:web_name %) "De Bruyne")
                                 player-expected-pts))]
    (is (= 6.250468904686143
           (:gw36-expected-points de-bruyne)))))

(deftest ings
  (let [ings (first (filter #(= (:web_name %) "Ings")
                            player-expected-pts))]
    (is (= 5.2187340114546075
           (:gw36-expected-points ings)))))

(deftest salah
  (let [salah (first (filter #(= (:web_name %) "Salah")
                            player-expected-pts))]
    (is (= 53.84574769669039
           (:expected-points-total salah)))))

(deftest calvert-lewin
  (let [calvert-lewin (first (filter #(= (:web_name %) "Calvert-Lewin")
                             player-expected-pts))]
    (is (= 45.674233466756434
           (:expected-points-total calvert-lewin)))))

(deftest azpilicueta
  (let [azpilicueta (first (filter #(= (:web_name %) "Azpilicueta")
                                     player-expected-pts))]
    (is (= 33.14782774532651
           (:expected-points-total azpilicueta)))))

(deftest schmeichel
  (let [schmeichel (first (filter #(= (:web_name %) "Schmeichel")
                                   player-expected-pts))]
    (is (= 23.48258303052768
           (:expected-points-total schmeichel)))))

(deftest mahrez
  (let [mahrez (first (filter #(= (:web_name %) "Mahrez")
                                  player-expected-pts))]
    (is (= 4.820538425556338
           (:expected_points_per_90 mahrez)))))

(deftest abraham
  (let [abraham (first (filter #(= (:web_name %) "Abraham")
                              player-expected-pts))]
    (is (= 4.319447377883253
           (:expected_points_per_90 abraham)))))

(deftest lundstram
  (let [lundstram (first (filter #(= (:web_name %) "Lundstram")
                               player-expected-pts))]
    (is (= 3.3297714914551815
           (:expected_points_per_90 lundstram)))))

(deftest patricio
  (let [patricio (first (filter #(= (:web_name %) "Patrício")
                                 player-expected-pts))]
    (is (= 2.5666140113056266
           (:expected_points_per_90 patricio)))))

(def player-expected-pts-double-gw
  (:elements (calculate-expected-values fpl-data fixtures 30 29 [30] :ignore-appearances false :test? true)))

(deftest mcburnie-double-gw
  (let [mcburnie (first (filter #(= (:web_name %) "McBurnie")
                                player-expected-pts-double-gw))]
    (is (= 8.456153623942509
           (:expected-points-total mcburnie)))))

(def player-expected-pts-blank
  (:elements (calculate-expected-values fpl-data fixtures 29 27 [28 29] :ignore-appearances false :test? true)))

(deftest mcburnie-blank
  (let [mcburnie (first (filter #(= (:web_name %) "McBurnie")
                                player-expected-pts-blank))]
    (is (= 0
           (:gw28-expected-points mcburnie)))
    (is (= 4.260282561449126
           (:expected-points-total mcburnie)))))
