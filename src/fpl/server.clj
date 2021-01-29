(ns fpl.server
  (:require
    [fpl.core-all-gws :refer [calculate-expected-values]]
    [ring.adapter.jetty :refer [run-jetty]]
    [clojure.data.json :as json]
    [clj-http.client :as client]
    [clojure.edn :as edn]
    [clojure.string :as string]
    [fpl.gw-data :refer [team-xg-diff-rank]])
  (:import (java.text Normalizer$Form Normalizer)))

(defn- get-fpl-data []
  (client/get "https://fantasy.premierleague.com/api/bootstrap-static/" {:as :json}))

(defn- get-fanteam-data []
  (client/get "https://fanteam-game.api.scoutgg.net/tournaments/291187/players"
              {:as :json
               :headers {"Authorization" "Bearer fanteam undefined"}}))

;(defn- get-fpl-data2 []
;  (json/read-str (slurp "../resources/fpl_data/test.json")
;                 :key-fn keyword))

(def fixtures
  (edn/read-string (slurp "../resources/fixtures.edn")))

(defn- filter-fpl-data [fpl-data]
  ; james rodriguez
  (filter #(= (:code %) 60025) fpl-data))

(def fpl-data
  (:elements (:body (get-fpl-data))))

(def fanteam-data
  (:body (get-fanteam-data)))

;(def fpl-data2
;  (get-fpl-data2))

(defn deaccent [str]
  "Remove accent from string"
  (let [normalized (Normalizer/normalize str Normalizer$Form/NFD)]
    (string/replace normalized #"\p{InCombiningDiacriticalMarks}+" "")))

(def fpl-to-fanteam-mapping
  [{:fpl-first-name "Joseph"
    :fpl-last-name "Willock"
    :fanteam-first-name "Joe"
    :fanteam-last-name "Willock"}
   {:fpl-first-name "Edward"
    :fpl-last-name "Nketiah"
    :fanteam-first-name "Eddie"
    :fanteam-last-name "Nketiah"}
   {:fpl-first-name "Ørjan"
   :fpl-last-name "Nyland"
   :fanteam-first-name "Örjan"
   :fanteam-last-name "Nyland"}
   {:fpl-first-name "Douglas Luiz"
    :fpl-last-name "Soares de Paulo"
    :fanteam-first-name "Douglas"
    :fanteam-last-name "Luiz"}
   {:fpl-first-name "Solomon"
    :fpl-last-name "March"
    :fanteam-first-name "Solly"
    :fanteam-last-name "March"}
   {:fpl-first-name "Alireza"
    :fpl-last-name "Jahanbakhsh"
    :fanteam-first-name "Alireza"
    :fanteam-last-name "Jahandakhsh"}
   {:fpl-first-name "Emerson"
    :fpl-last-name "Palmieri dos Santos"
    :fanteam-first-name "Palmieri"
    :fanteam-last-name "Emerson"}
   {:fpl-first-name "Patrick"
    :fpl-last-name "van Aanholt"
    :fanteam-first-name "Patrick van"
    :fanteam-last-name "Aanholt"}
   {:fpl-first-name "Wilfred"
    :fpl-last-name "Ndidi"
    :fanteam-first-name "Onyinye Wilfred"
    :fanteam-last-name "Ndidi"}
   {:fpl-first-name "Virgil"
    :fpl-last-name "van Dijk"
    :fanteam-first-name "Virgil van"
    :fanteam-last-name "Dijk"}
   {:fpl-first-name "Alisson"
    :fpl-last-name "Ramses Becker"
    :fanteam-first-name "Becker"
    :fanteam-last-name "Alisson"}
   {:fpl-first-name "Joseph"
    :fpl-last-name "Gomez"
    :fanteam-first-name "Joe"
    :fanteam-last-name "Gomez"}
   {:fpl-first-name "Joelinton Cássio"
    :fpl-last-name "Apolinário de Lira"
    :fanteam-first-name ""
    :fanteam-last-name "Joelinton"}
   {:fpl-first-name "Sean"
    :fpl-last-name "Longstaff"
    :fanteam-first-name "Sean"
    :fanteam-last-name "S. Longstaff"}
   {:fpl-first-name "Bamidele"
    :fpl-last-name "Alli"
    :fanteam-first-name "Dele"
    :fanteam-last-name "Alli"}
   {:fpl-first-name "Pierre-Emile"
    :fpl-last-name "Højbjerg"
    :fanteam-first-name "Pierre-Emile"
    :fanteam-last-name "Höjbjerg"}
   {:fpl-first-name "Oluwasemilogo Adesewo Ibidapo"
    :fpl-last-name "Ajayi"
    :fanteam-first-name "Semi"
    :fanteam-last-name "Ajayi"}
   {:fpl-first-name "Felipe Anderson"
    :fpl-last-name "Pereira Gomes"
    :fanteam-first-name "Felipe"
    :fanteam-last-name "Anderson"}])

; unknown in fanteam:
; Emile Smith Rowe
; James Bree
; Aaron Mooy
; Martín Montoya
; Florin Andone
; José Izquierdo
; Matthew Clarke
; Jorge Luiz Frello Filho
; Cenk Tosun
; Fabricio Agosto Ramírez
; Adrien Silva
; Filip Benkovic
; Francisco Casilla Cortés
; Kamil Miazek
; Oliver Casey
; Fabio Henrique Tavares
; Harry Wilson
; Claudio Bravo
; Tommy Doyle
; Marcos Rojo
; Frederico Rodrigues de Paula Santos
; Tahith Chong
; Teden Mengi
; Rolando Aarons
; Kelland Watts
; Matthew Longstaff
; Mark Gillespie
; Luke Freeman
; Josh Sims
; Oliver Skipp
; Jonathan Leko
; Roberto Jimenez Gago
; Jordan Hugill
; Albian Ajeti
; Jonathan Castro Otto
; Léo Bonatini
; Oskar Buur
; Matija Sarkic
; Morgan Gibbs-White

(defn- check-mapping [fpl-player-first-name fpl-player-last-name fanteam-player-first-name fanteam-player-last-name]
  (first (filter (fn [{:keys [fpl-first-name fpl-last-name fanteam-first-name fanteam-last-name]}]
                   (and (= fpl-player-first-name fpl-first-name)
                        (= fpl-player-last-name fpl-last-name)
                        (= fanteam-player-first-name fanteam-first-name)
                        (= fanteam-player-last-name fanteam-last-name))) fpl-to-fanteam-mapping)))

(defn- find-fanteam-player [fpl-player]
  (let [fpl-player-first-name (:first_name fpl-player)
        fpl-player-first-name-deaccent (deaccent fpl-player-first-name)
        fpl-player-last-name (:second_name fpl-player)
        fpl-player-last-name-deaccent (deaccent fpl-player-last-name)
        fpl-web-name (:web_name fpl-player)
        fpl-web-name-deaccent (deaccent fpl-web-name)]
    (->> fanteam-data
         :playerChoices
         (filter (fn [fanteam-player]
                   (let [fanteam-player-first-name (-> fanteam-player :realPlayer :firstName)
                         fanteam-player-first-name-trimmed (if (nil? fanteam-player-first-name)
                                                             fanteam-player-first-name
                                                             (string/trim fanteam-player-first-name))
                         fanteam-player-first-name-deaccent (if (nil? fanteam-player-first-name)
                                                              fanteam-player-first-name-trimmed
                                                              (deaccent fanteam-player-first-name-trimmed))
                         fanteam-player-last-name (string/trim (-> fanteam-player :realPlayer :lastName))
                         fanteam-player-last-name-deaccent (deaccent fanteam-player-last-name)]
                     ;(when (or (nil? fanteam-player-first-name)
                     ;          (nil? fanteam-player-last-name))
                     ;  (println (str fanteam-player-first-name " " fanteam-player-last-name)))
                     ;(when (and (= fpl-player-last-name "Nyland")
                     ;           (= (-> fanteam-player :realPlayer :lastName) "Nyland"))
                     ;  (println ""))
                     (cond
                       (and (nil? fanteam-player-first-name-deaccent)
                            (not (string/includes? fpl-web-name-deaccent fanteam-player-last-name-deaccent))) false
                       (and (nil? fanteam-player-first-name-deaccent)
                            (string/includes? fpl-web-name-deaccent fanteam-player-last-name-deaccent)) true
                       (and (string/includes? fpl-player-first-name-deaccent fanteam-player-first-name-deaccent)
                            (string/includes? fpl-player-last-name-deaccent fanteam-player-last-name-deaccent)) true
                       (and (check-mapping fpl-player-first-name
                                           fpl-player-last-name
                                           fanteam-player-first-name-trimmed
                                           fanteam-player-last-name)) true
                       :else false))))
         first)))

(def merge-fpl-and-fanteam-data
  (map (fn [fpl-player]
         (let [fanteam-player (find-fanteam-player fpl-player)]
           (when (nil? fanteam-player)
              (println (str "could not match fpl player: " (:first_name fpl-player) " " (:second_name fpl-player))))
           (if-not (nil? fanteam-player)
             (assoc fpl-player :now_cost (* (:price fanteam-player) 10))
             fpl-player)))
       fpl-data))

(def results-fpl
  (calculate-expected-values fpl-data fixtures 20 20 [21 22 23] :ignore-appearances true :test?
                                              false))

;(def results-fan-team
;  (calculate-expected-values merge-fpl-and-fanteam-data fixtures 20 20 [21 22 23] :ignore-appearances
;                             true
;                             :test?
;                             false))

(defn handler [request]
  {:status  200
   :headers {"Content-Type" "application/json"}
   :body    (if (= (:uri request) "/teams")
              (json/write-str team-xg-diff-rank)
              (json/write-str results-fpl))})

(defn -main [] (run-jetty handler {:port 3000}))
