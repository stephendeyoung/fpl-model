(ns fpl.gw-data
  (:require [clojure.edn :as edn]))

(defn- read-statsbomb-data [filename]
  (edn/read-string (slurp (str "../resources/" filename))))

(def gameweek-data
  [{:gw          9
    :player-data (read-statsbomb-data "statsbomb-player-data-gw9.edn")
    :team-data   (read-statsbomb-data "statsbomb-team-data-gw9.edn")}
   {:gw          10
    :player-data (read-statsbomb-data "statsbomb-player-data-gw10.edn")
    :team-data   (read-statsbomb-data "statsbomb-team-data-gw10.edn")}
   {:gw          11
    :player-data (read-statsbomb-data "statsbomb-player-data-gw11.edn")
    :team-data   (read-statsbomb-data "statsbomb-team-data-gw11.edn")}
   {:gw          12
    :player-data (read-statsbomb-data "statsbomb-player-data-gw12.edn")
    :team-data   (read-statsbomb-data "statsbomb-team-data-gw12.edn")}
   {:gw          13
    :player-data (read-statsbomb-data "statsbomb-player-data-gw13.edn")
    :team-data   (read-statsbomb-data "statsbomb-team-data-gw13.edn")}
   {:gw          14
    :player-data (read-statsbomb-data "statsbomb-player-data-gw14.edn")
    :team-data   (read-statsbomb-data "statsbomb-team-data-gw14.edn")}
   {:gw          15
    :player-data (read-statsbomb-data "statsbomb-player-data-gw15.edn")
    :team-data   (read-statsbomb-data "statsbomb-team-data-gw15.edn")}
   {:gw          16
    :player-data (read-statsbomb-data "statsbomb-player-data-gw16.edn")
    :team-data   (read-statsbomb-data "statsbomb-team-data-gw16.edn")}
   {:gw          17
    :player-data (read-statsbomb-data "statsbomb-player-data-gw17.edn")
    :team-data   (read-statsbomb-data "statsbomb-team-data-gw17.edn")}
   {:gw          18
    :player-data (read-statsbomb-data "statsbomb-player-data-gw18.edn")
    :team-data   (read-statsbomb-data "statsbomb-team-data-gw18.edn")}
   {:gw          19
    :player-data (read-statsbomb-data "statsbomb-player-data-gw19.edn")
    :team-data   (read-statsbomb-data "statsbomb-team-data-gw19.edn")}
   {:gw          20
    :player-data (read-statsbomb-data "statsbomb-player-data-gw20.edn")
    :team-data   (read-statsbomb-data "statsbomb-team-data-gw20.edn")}
   {:gw          21
    :player-data (read-statsbomb-data "statsbomb-player-data-gw21.edn")
    :team-data   (read-statsbomb-data "statsbomb-team-data-gw21.edn")}
   {:gw          22
    :player-data (read-statsbomb-data "statsbomb-player-data-gw22.edn")
    :team-data   (read-statsbomb-data "statsbomb-team-data-gw22.edn")}
   {:gw          23
    :player-data (read-statsbomb-data "statsbomb-player-data-gw23.edn")
    :team-data   (read-statsbomb-data "statsbomb-team-data-gw23.edn")}
   {:gw          24
    :player-data (read-statsbomb-data "statsbomb-player-data-gw24.edn")
    :team-data   (read-statsbomb-data "statsbomb-team-data-gw24.edn")}
   {:gw          25
    :player-data (read-statsbomb-data "statsbomb-player-data-gw25.edn")
    :team-data   (read-statsbomb-data "statsbomb-team-data-gw25.edn")}
   {:gw          26
    :player-data (read-statsbomb-data "statsbomb-player-data-gw26.edn")
    :team-data   (read-statsbomb-data "statsbomb-team-data-gw26.edn")}
   {:gw          27
    :player-data (read-statsbomb-data "statsbomb-player-data-gw27.edn")
    :team-data   (read-statsbomb-data "statsbomb-team-data-gw27.edn")}
   {:gw          28
    :player-data (read-statsbomb-data "statsbomb-player-data-gw28.edn")
    :team-data   (read-statsbomb-data "statsbomb-team-data-gw28.edn")}
   {:gw          29
    :player-data (read-statsbomb-data "statsbomb-player-data-gw29.edn")
    :team-data   (read-statsbomb-data "statsbomb-team-data-gw29.edn")}
   ])

(defn home-away-data [gw]
  {:player-data-home (read-statsbomb-data (str "home_away_data/statsbomb-player-data-home-gw" gw ".edn"))
   :player-data-away (read-statsbomb-data (str "home_away_data/statsbomb-player-data-away-gw" gw ".edn"))
   :team-data-home   (read-statsbomb-data (str "home_away_data/statsbomb-team-data-home-gw" gw ".edn"))
   :team-data-away   (read-statsbomb-data (str "home_away_data/statsbomb-team-data-away-gw" gw ".edn"))})

(defn fixture-data [fixtures fixtures-to-retrieve test?]
  (filter #(and (>= (:gw %) (first fixtures-to-retrieve))
                (= (:test? %) test?))
          [
           ;{:gw 26
           ; :fixtures (filter (fn [fixture]
           ;                     (and (.after (:date fixture) #inst "2020-02-08")
           ;                          (.before (:date fixture) #inst "2020-02-18")))
           ;                   fixtures)}
           ;{:gw 27
           ; :fixtures (filter (fn [fixture]
           ;                     (and (.after (:date fixture) #inst "2020-02-22")
           ;                          (.before (:date fixture) #inst "2020-02-25")))
           ;                   fixtures)}
           {:gw 28
            :fixtures (filter (fn [fixture]
                                (and (.after (:date fixture) #inst "2020-02-28")
                                     (.before (:date fixture) #inst "2020-03-02")))
                              fixtures)
            :test? true
            :blanks [102 247 152 178]}
           {:gw 29
            :fixtures (filter (fn [fixture]
                                (and (.after (:date fixture) #inst "2020-03-07")
                                     (.before (:date fixture) #inst "2020-03-12")))
                              fixtures)
            :test? true
            :doubles [{:home-team-id 247
                       :away-team-id 178}]}
           {:gw       30
            :fixtures (filter (fn [fixture]
                                (and (.after (:date fixture) #inst "2020-03-14")
                                     (.before (:date fixture) #inst "2020-03-17")))
                              fixtures)
            :test? true
            :doubles [{:home-team-id 247
                       :away-team-id 178}
                      {:home-team-id 152
                       :away-team-id 102}]}
           {:gw       31
            :test? true
            :fixtures (filter (fn [fixture]
                                (and (.after (:date fixture) #inst "2020-03-20")
                                     (.before (:date fixture) #inst "2020-03-23")))
                              fixtures)
            ;:blanks   [247 178 666 168 152 122 88 754 102 110 199 118]
            }
           {:gw       32
            :test? true
            :fixtures (filter (fn [fixture]
                                (and (.after (:date fixture) #inst "2020-04-04")
                                     (.before (:date fixture) #inst "2020-04-05")))
                              fixtures)}
           {:gw       33
            :test? true
            :fixtures (filter (fn [fixture]
                                (and (.after (:date fixture) #inst "2020-04-11")
                                     (.before (:date fixture) #inst "2020-04-12")))
                              fixtures)}
           {:gw       34
            :test? true
            :fixtures (filter (fn [fixture]
                                (and (.after (:date fixture) #inst "2020-04-18")
                                     (.before (:date fixture) #inst "2020-04-19")))
                              fixtures)}
           {:gw       35
            :test? true
            :fixtures (filter (fn [fixture]
                                (and (.after (:date fixture) #inst "2020-04-25")
                                     (.before (:date fixture) #inst "2020-04-26")))
                              fixtures)}
           {:gw       36
            :test? true
            :fixtures (filter (fn [fixture]
                                (and (.after (:date fixture) #inst "2020-05-02")
                                     (.before (:date fixture) #inst "2020-05-03")))
                              fixtures)}
           {:gw       37
            :test? true
            :fixtures (filter (fn [fixture]
                                (and (.after (:date fixture) #inst "2020-05-09")
                                     (.before (:date fixture) #inst "2020-05-10")))
                              fixtures)}
           {:gw       38
            :test? true
            :fixtures (filter (fn [fixture]
                                (and (.after (:date fixture) #inst "2020-05-17")
                                     (.before (:date fixture) #inst "2020-05-18")))
                              fixtures)}
           {:gw       31
            :test? false
            :fixtures (filter (fn [fixture]
                                (and (.after (:date fixture) #inst "2020-06-23")
                                     (.before (:date fixture) #inst "2020-06-26")))
                              fixtures)}
           {:gw       32
            :test? false
            :fixtures (filter (fn [fixture]
                                (and (.after (:date fixture) #inst "2020-06-27")
                                     (.before (:date fixture) #inst "2020-07-03")))
                              fixtures)}
           {:gw       33
            :test? false
            :fixtures (filter (fn [fixture]
                                (and (.after (:date fixture) #inst "2020-07-04")
                                     (.before (:date fixture) #inst "2020-07-07")))
                              fixtures)}
           {:gw       34
            :test? false
            :fixtures (filter (fn [fixture]
                                (and (.after (:date fixture) #inst "2020-07-07")
                                     (.before (:date fixture) #inst "2020-07-10")))
                              fixtures)}
           {:gw       35
            :test? false
            :fixtures (filter (fn [fixture]
                                (and (.after (:date fixture) #inst "2020-07-11")
                                     (.before (:date fixture) #inst "2020-07-14")))
                              fixtures)}
           {:gw       36
            :test? false
            :fixtures (filter (fn [fixture]
                                (and (.after (:date fixture) #inst "2020-07-15")
                                     (.before (:date fixture) #inst "2020-07-16")))
                              fixtures)}
           {:gw       37
            :test? false
            :fixtures (filter (fn [fixture]
                                (and (.after (:date fixture) #inst "2020-07-18")
                                     (.before (:date fixture) #inst "2020-07-19")))
                              fixtures)}
           {:gw       38
            :test? false
            :fixtures (filter (fn [fixture]
                                (and (.after (:date fixture) #inst "2020-07-26")
                                     (.before (:date fixture) #inst "2020-07-27")))
                              fixtures)}
           ]))