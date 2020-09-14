(ns fpl.gw-data
  (:require [clojure.edn :as edn]))

(defn- read-statsbomb-data [filename]
  (edn/read-string (slurp (str "../resources/" filename))))

;(def base-weight-total 0.0175)
;(def base-weight-home-away 0.035)

(def min-range-mins 1)
(def max-range-mins-home-away 22.22)
(def max-range-mins-total 42.22)
(def new-data-weight 0.1)

(defn- ema3 [c a]
  (loop [ct (rest c) res [(first c)]]
    (if (= (count ct) 0)
      res
      (recur
        (rest ct)
        (into
          res
          [(+ (* a (first ct)) (* (- 1 a) (peek res)))])))))

(defn- merge-gw-player-data [gw-data prev-season-data max-range-mins]
  (map (fn [data]
         (let [player-in-prev-season (first (filter #(= (:player_id %) (:player_id data)) prev-season-data))
               minutes (:player_season_minutes data)
               proportion-mins-prev-season (if (nil? player-in-prev-season)
                                             0
                                             (/ minutes (:player_season_minutes
                                                          player-in-prev-season)))
               weight (if (< proportion-mins-prev-season 1)
                        (+ (/ proportion-mins-prev-season 2)
                           new-data-weight)
                        ; (11.1 − 1) / (22.22 − 1) × (1 − 0.5) + 0.5
                        (+ (* (/ (- proportion-mins-prev-season min-range-mins) (- max-range-mins min-range-mins))
                              (- 0.97 (+ 0.5 new-data-weight)))
                           (+ 0.5 new-data-weight)))
               {player_season_xa_90    :player_season_xa_90
                player_season_np_xg_90 :player_season_np_xg_90
                player_season_gsaa_90  :player_season_gsaa_90} data]
           (merge (select-keys data [:player_id :player_opta_id :team_id :team_opta_id :player_last_name])
                  {:player_season_xa_90    (cond
                                             (nil? player-in-prev-season) player_season_xa_90
                                             (and (not= player-in-prev-season nil)
                                                  (= minutes 0)) (:player_season_xa_90 player-in-prev-season)
                                             :else (last (ema3 [(:player_season_xa_90 player-in-prev-season)
                                                                player_season_xa_90]
                                                               weight)))
                   :player_season_np_xg_90 (cond
                                             (nil? player-in-prev-season) player_season_np_xg_90
                                             (and (not= player-in-prev-season nil)
                                                  (= minutes 0)) (:player_season_np_xg_90 player-in-prev-season)
                                             :else (last (ema3 [(:player_season_np_xg_90 player-in-prev-season)
                                                                player_season_np_xg_90]
                                                               weight)))
                   :player_season_gsaa_90  (cond
                                             (nil? player-in-prev-season) player_season_gsaa_90
                                             (and (not= player-in-prev-season nil)
                                                  (= minutes 0)) (:player_season_gsaa_90 player-in-prev-season)
                                             :else (last (ema3 [(:player_season_gsaa_90 player-in-prev-season)
                                                                player_season_gsaa_90]
                                                               weight)))
                   :player_season_minutes  (cond
                                             (nil? player-in-prev-season) minutes
                                             (and (not= player-in-prev-season nil)
                                                  (= minutes 0)) (:player_season_minutes player-in-prev-season)
                                             :else (+ minutes
                                                      (:player_season_minutes player-in-prev-season)))})))
       gw-data))

(defn- merge-gw-team-data [gw-data prev-season-data]
  (map (fn [data]
         (let [team-in-prev-season (first (filter #(= (:team_id %) (:team_id data)) prev-season-data))
               matches (:team_season_matches data)
               proportion-matches-prev-season (/ matches (:team_season_matches team-in-prev-season))
               weight (+ (/ proportion-matches-prev-season 2)
                         new-data-weight)
               {team_season_np_xg_conceded_pg :team_season_np_xg_conceded_pg
                team_season_np_xg_pg          :team_season_np_xg_pg} data]
           (merge (select-keys data [:team_id :team_opta_id :team_season_matches])
                  {:team_season_np_xg_conceded_pg (last (ema3 [(:team_season_np_xg_conceded_pg team-in-prev-season)
                                                               team_season_np_xg_conceded_pg]
                                                              weight))
                   :team_season_np_xg_pg          (last (ema3 [(:team_season_np_xg_pg team-in-prev-season)
                                                               team_season_np_xg_pg]
                                                              weight))})))
       gw-data))

(def prev-season-data
  {:player-data      (read-statsbomb-data "statsbomb-player-data-gw38.edn")
   :team-data        (read-statsbomb-data "statsbomb-team-data-gw38.edn")
   :player-data-home (read-statsbomb-data (str "home_away_data/statsbomb-player-data-home-gw38.edn"))
   :player-data-away (read-statsbomb-data (str "home_away_data/statsbomb-player-data-away-gw38.edn"))
   :team-data-home   (read-statsbomb-data (str "home_away_data/statsbomb-team-data-home-gw38.edn"))
   :team-data-away   (read-statsbomb-data (str "home_away_data/statsbomb-team-data-away-gw38.edn"))})

(def gameweek-data
  [{:gw          38
    :player-data (merge-gw-player-data (read-statsbomb-data "statsbomb-player-data-gw38.edn")
                                       (:player-data prev-season-data)
                                       max-range-mins-total)
    :team-data   (merge-gw-team-data (read-statsbomb-data "statsbomb-team-data-gw38.edn")
                                     (:team-data prev-season-data))}])

(defn home-away-data [gw]
  {:player-data-home (merge-gw-player-data (read-statsbomb-data (str "home_away_data/statsbomb-player-data-home-gw" gw ".edn"))
                                           (:player-data-home prev-season-data)
                                           max-range-mins-home-away)
   :player-data-away (merge-gw-player-data (read-statsbomb-data (str "home_away_data/statsbomb-player-data-away-gw" gw ".edn"))
                                           (:player-data-away prev-season-data)
                                           max-range-mins-home-away)
   :team-data-home   (merge-gw-team-data (read-statsbomb-data (str "home_away_data/statsbomb-team-data-home-gw" gw ".edn"))
                                         (:team-data-home prev-season-data))
   :team-data-away   (merge-gw-team-data (read-statsbomb-data (str "home_away_data/statsbomb-team-data-away-gw" gw ".edn"))
                                         (:team-data-away prev-season-data))})

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
           ;{:gw 28
           ; :fixtures (filter (fn [fixture]
           ;                     (and (.after (:date fixture) #inst "2020-02-28")
           ;                          (.before (:date fixture) #inst "2020-03-02")))
           ;                   fixtures)
           ; :test? true
           ; :blanks [102 247 152 178]}
           ;{:gw 29
           ; :fixtures (filter (fn [fixture]
           ;                     (and (.after (:date fixture) #inst "2020-03-07")
           ;                          (.before (:date fixture) #inst "2020-03-12")))
           ;                   fixtures)
           ; :test? true
           ; :doubles [{:home-team-id 247
           ;            :away-team-id 178}]}
           ;{:gw       30
           ; :fixtures (filter (fn [fixture]
           ;                     (and (.after (:date fixture) #inst "2020-03-14")
           ;                          (.before (:date fixture) #inst "2020-03-17")))
           ;                   fixtures)
           ; :test? true
           ; :doubles [{:home-team-id 247
           ;            :away-team-id 178}
           ;           {:home-team-id 152
           ;            :away-team-id 102}]}
           ;{:gw       31
           ; :test? true
           ; :fixtures (filter (fn [fixture]
           ;                     (and (.after (:date fixture) #inst "2020-03-20")
           ;                          (.before (:date fixture) #inst "2020-03-23")))
           ;                   fixtures)
           ; ;:blanks   [247 178 666 168 152 122 88 754 102 110 199 118]
           ; }
           ;{:gw       32
           ; :test? true
           ; :fixtures (filter (fn [fixture]
           ;                     (and (.after (:date fixture) #inst "2020-04-04")
           ;                          (.before (:date fixture) #inst "2020-04-05")))
           ;                   fixtures)}
           ;{:gw       33
           ; :test? true
           ; :fixtures (filter (fn [fixture]
           ;                     (and (.after (:date fixture) #inst "2020-04-11")
           ;                          (.before (:date fixture) #inst "2020-04-12")))
           ;                   fixtures)}
           ;{:gw       34
           ; :test? true
           ; :fixtures (filter (fn [fixture]
           ;                     (and (.after (:date fixture) #inst "2020-04-18")
           ;                          (.before (:date fixture) #inst "2020-04-19")))
           ;                   fixtures)}
           ;{:gw       35
           ; :test? true
           ; :fixtures (filter (fn [fixture]
           ;                     (and (.after (:date fixture) #inst "2020-04-25")
           ;                          (.before (:date fixture) #inst "2020-04-26")))
           ;                   fixtures)}
           ;{:gw       36
           ; :test? true
           ; :fixtures (filter (fn [fixture]
           ;                     (and (.after (:date fixture) #inst "2020-05-02")
           ;                          (.before (:date fixture) #inst "2020-05-03")))
           ;                   fixtures)}
           ;{:gw       37
           ; :test? true
           ; :fixtures (filter (fn [fixture]
           ;                     (and (.after (:date fixture) #inst "2020-05-09")
           ;                          (.before (:date fixture) #inst "2020-05-10")))
           ;                   fixtures)}
           ;{:gw       38
           ; :test? true
           ; :fixtures (filter (fn [fixture]
           ;                     (and (.after (:date fixture) #inst "2020-05-17")
           ;                          (.before (:date fixture) #inst "2020-05-18")))
           ;                   fixtures)}
           ;{:gw       31
           ; :test? false
           ; :fixtures (filter (fn [fixture]
           ;                     (and (.after (:date fixture) #inst "2020-06-23")
           ;                          (.before (:date fixture) #inst "2020-06-26")))
           ;                   fixtures)}
           ;{:gw       32
           ; :test? false
           ; :fixtures (filter (fn [fixture]
           ;                     (and (.after (:date fixture) #inst "2020-06-27")
           ;                          (.before (:date fixture) #inst "2020-07-03")))
           ;                   fixtures)}
           ;{:gw       33
           ; :test? false
           ; :fixtures (filter (fn [fixture]
           ;                     (and (.after (:date fixture) #inst "2020-07-04")
           ;                          (.before (:date fixture) #inst "2020-07-07")))
           ;                   fixtures)}
           ;{:gw       34
           ; :test? false
           ; :fixtures (filter (fn [fixture]
           ;                     (and (.after (:date fixture) #inst "2020-07-07")
           ;                          (.before (:date fixture) #inst "2020-07-10")))
           ;                   fixtures)}
           ;{:gw       35
           ; :test? false
           ; :fixtures (filter (fn [fixture]
           ;                     (and (.after (:date fixture) #inst "2020-07-11")
           ;                          (.before (:date fixture) #inst "2020-07-14")))
           ;                   fixtures)}
           ;{:gw       36
           ; :test? false
           ; :fixtures (filter (fn [fixture]
           ;                     (and (.after (:date fixture) #inst "2020-07-15")
           ;                          (.before (:date fixture) #inst "2020-07-16")))
           ;                   fixtures)}
           ;{:gw       37
           ; :test? false
           ; :fixtures (filter (fn [fixture]
           ;                     (and (.after (:date fixture) #inst "2020-07-18")
           ;                          (.before (:date fixture) #inst "2020-07-19")))
           ;                   fixtures)}
           ;{:gw       38
           ; :test? false
           ; :fixtures (filter (fn [fixture]
           ;                     (and (.after (:date fixture) #inst "2020-07-26")
           ;                          (.before (:date fixture) #inst "2020-07-27")))
           ;                   fixtures)}
           {:gw       1
            :test?    false
            :fixtures (filter (fn [fixture]
                                (and (.after (:date fixture) #inst "2020-09-12")
                                     (.before (:date fixture) #inst "2020-09-15")))
                              fixtures)
            :blanks   [1 43 90 7 45 57 91]}
           {:gw       2
            :test?    false
            :fixtures (filter (fn [fixture]
                                (and (.after (:date fixture) #inst "2020-09-19")
                                     (.before (:date fixture) #inst "2020-09-22")))
                              fixtures)
            :blanks   [45 57 91]}
           {:gw       3
            :test?    false
            :fixtures (filter (fn [fixture]
                                (and (.after (:date fixture) #inst "2020-09-26")
                                     (.before (:date fixture) #inst "2020-09-29")))
                              fixtures)
            :blanks   [45 57 91]}
           {:gw       4
            :test?    false
            :fixtures (filter (fn [fixture]
                                (and (.after (:date fixture) #inst "2020-10-03")
                                     (.before (:date fixture) #inst "2020-10-04")))
                              fixtures)
            :blanks   [45 57 91]}
           {:gw       5
            :test?    false
            :fixtures (filter (fn [fixture]
                                (and (.after (:date fixture) #inst "2020-10-17")
                                     (.before (:date fixture) #inst "2020-10-18")))
                              fixtures)
            :blanks   [45 57 91]}
           {:gw       6
            :test?    false
            :fixtures (filter (fn [fixture]
                                (and (.after (:date fixture) #inst "2020-10-24")
                                     (.before (:date fixture) #inst "2020-10-25")))
                              fixtures)
            :blanks   [45 57 91]}
           {:gw       7
            :test?    false
            :fixtures (filter (fn [fixture]
                                (and (.after (:date fixture) #inst "2020-10-31")
                                     (.before (:date fixture) #inst "2020-11-01")))
                              fixtures)
            :blanks   [45 57 91]}
           {:gw       8
            :test?    false
            :fixtures (filter (fn [fixture]
                                (and (.after (:date fixture) #inst "2020-11-07")
                                     (.before (:date fixture) #inst "2020-11-08")))
                              fixtures)
            :blanks   [45 57 91]}
           ]))