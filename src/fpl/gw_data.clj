(ns fpl.gw-data
  (:require [clojure.edn :as edn]
            [clojure.pprint :as pprint]))

(defn- read-statsbomb-data [filename]
  (edn/read-string (slurp (str "../resources/" filename))))

;(def base-weight-total 0.0175)
;(def base-weight-home-away 0.035)

(def max-range-mins-home-away 1860)
(def max-range-mins-total 3715)
(def new-data-weight 0.5)

(defn- ema3 [c a]
  (loop [ct (rest c) res [(first c)]]
    (if (= (count ct) 0)
      res
      (recur
        (rest ct)
        (into
          res
          [(+ (* a (first ct)) (* (- 1 a) (peek res)))])))))

(defn- merge-gw-player-data [gw-data prev-season-data max-possible-mins type & [latest?]]
  (let [missing-players-prev-season (filter (fn [player]
                                              (let [player-in-gw (first (filter #(= (:player_id %) (:player_id player))
                                                                                gw-data))]
                                                (nil? player-in-gw)))
                                            prev-season-data)]
    (map (fn [data]
           (let [player-in-prev-season (first (filter #(= (:player_id %) (:player_id data)) prev-season-data))
                 minutes (:player_season_minutes data)
                 minutes-prev-season (:player_season_minutes player-in-prev-season)
                 proportion-mins-prev-season (if (nil? player-in-prev-season)
                                               0
                                               (/ minutes minutes-prev-season))
                 weight (if (< proportion-mins-prev-season 1)
                          (+ (/ proportion-mins-prev-season 2)
                             new-data-weight)
                          ; (((3715 / 1275) - 1) / ((3715 / 1275) - 1)) * (1 - 0.6) + 0.6
                          (+ (* (/ (- proportion-mins-prev-season 1) (- (/ max-possible-mins minutes-prev-season) 1))
                                0.6)
                             0.6))
                 {player_season_xa_90    :player_season_xa_90
                  player_season_np_xg_90 :player_season_np_xg_90
                  player_season_gsaa_90  :player_season_gsaa_90} data
                 weighted-xa-per-90 (cond
                                      (nil? player-in-prev-season) player_season_xa_90
                                      (and (not= player-in-prev-season nil)
                                           (= minutes 0)) (:player_season_xa_90 player-in-prev-season)
                                      :else (last (ema3 [(:player_season_xa_90 player-in-prev-season)
                                                         player_season_xa_90]
                                                        weight)))
                 weighted-xg-per-90 (cond
                                      (nil? player-in-prev-season) player_season_np_xg_90
                                      (and (not= player-in-prev-season nil)
                                           (= minutes 0)) (:player_season_np_xg_90 player-in-prev-season)
                                      :else (last (ema3 [(:player_season_np_xg_90 player-in-prev-season)
                                                         player_season_np_xg_90]
                                                        weight)))]
             (when (> weight 1)
               (throw (ex-info (str (:name data) "'s weight is greater than 1")
                               {:weight weight
                                :proportion-mins-prev-season proportion-mins-prev-season
                                :minutes minutes
                                :minutes-prev-season minutes-prev-season
                                :max-mins max-possible-mins})))
             (when (or #_(= "Mohamed Salah" (:name data))
                       #_(= "Harry Kane" (:name data))
                       #_(= "Timo Werner" (:name data))
                       #_(= "Raheem Sterling" (:name data))
                       (= "Christian Pulisic" (:name data))
                       #_(= "Bruno Fernandes" (:name data))
                       #_(= "Che Adams" (:name data))
                       #_(= "Jarrod Bowen" (:name data)))
               (println (str (:name data) " " type " latest: " (if latest? true false)))
               (println "prev player_season_np_xg_90" (:player_season_np_xg_90 player-in-prev-season))
               (println "prev player_season_xa_90" (:player_season_xa_90 player-in-prev-season))
               (println "current player_season_np_xg_90" player_season_np_xg_90)
               (println "current player_season_xa_90" player_season_xa_90)
               (println "weighted xg-per-90" weighted-xg-per-90)
               (println "weighted xa-per-90" weighted-xa-per-90)
               (println "minutes" minutes)
               (println "minutes-prev-season" minutes-prev-season)
               (println "proportion-mins-prev-season" proportion-mins-prev-season)
               (println "weight" weight)
               (println "max-possible-mins" max-possible-mins)
               (println ""))
             (merge (select-keys data [:player_id :player_opta_id :team_id :team_opta_id :player_last_name])
                    {:player_season_xa_90 weighted-xa-per-90
                     :player_season_np_xg_90 weighted-xg-per-90
                     :player_season_gsaa_90  (cond
                                               (nil? player-in-prev-season) player_season_gsaa_90
                                               (and (not= player-in-prev-season nil)
                                                    (= minutes 0)) (:player_season_gsaa_90 player-in-prev-season)
                                               :else (last (ema3 [(:player_season_gsaa_90 player-in-prev-season)
                                                                  player_season_gsaa_90]
                                                                 weight)))
                     :player_season_minutes  (if (and latest? (= "both" type))
                                               minutes
                                               nil)})))
         (concat gw-data missing-players-prev-season))))

(defn- merge-gw-team-data [gw-data prev-season-data type & [latest?]]
  (map (fn [data]
         (if-let [team-in-gw (first (filter #(= (:team_id %) (:team_id data)) gw-data))]
           (let [matches (:team_season_matches data)
                 matches-so-far (:team_season_matches team-in-gw)
                 proportion-matches-prev-season (/ matches-so-far matches)
                 weight (+ (/ proportion-matches-prev-season 2)
                           new-data-weight)
                 {team_season_np_xg_conceded_pg :team_season_np_xg_conceded_pg
                  team_season_np_xg_pg          :team_season_np_xg_pg} data
                 weighted-team-season-xg-conceded (last (ema3 [team_season_np_xg_conceded_pg
                                                               (:team_season_np_xg_conceded_pg team-in-gw)]
                                                              weight))
                 weighted-team-season-xg (last (ema3 [team_season_np_xg_pg
                                                      (:team_season_np_xg_pg team-in-gw)]
                                                     weight))]
             (when (nil? team-in-gw)
               (println data))
             (println (str (:team_name data) " " type " latest: " (if latest? true false)))
             (println "prev team_season_np_xg_pg" team_season_np_xg_pg)
             (println "prev team_season_np_xg_conceded_pg" team_season_np_xg_conceded_pg)
             (println "current team_season_np_xg_pg" (:team_season_np_xg_pg team-in-gw))
             (println "current team_season_np_xg_conceded_pg" (:team_season_np_xg_conceded_pg team-in-gw))
             (println "weighted-team-season-xg" weighted-team-season-xg)
             (println "weighted-team-season-xg-conceded" weighted-team-season-xg-conceded)
             (println "matches-so-far" matches-so-far)
             (println "proportion-matches-prev-season" proportion-matches-prev-season)
             (println "weight" weight)
             (println "")
             (merge (select-keys data [:team_id :team_name :team_opta_id :team_season_matches])
                    {:team_season_np_xg_conceded_pg weighted-team-season-xg-conceded
                     :team_season_np_xg_pg          weighted-team-season-xg}))
           data))
       prev-season-data))

(def prev-season-data
  {:player-data      (read-statsbomb-data "19-20-data/statsbomb-player-data-gw38.edn")
   :team-data        (read-statsbomb-data "19-20-data/statsbomb-team-data-gw38.edn")
   :player-data-home (read-statsbomb-data (str "19-20-data/home_away_data/statsbomb-player-data-home-gw38.edn"))
   :player-data-away (read-statsbomb-data (str "19-20-data/home_away_data/statsbomb-player-data-away-gw38.edn"))
   :team-data-home   (read-statsbomb-data (str "19-20-data/home_away_data/statsbomb-team-data-home-gw38.edn"))
   :team-data-away   (read-statsbomb-data (str "19-20-data/home_away_data/statsbomb-team-data-away-gw38.edn"))})

(def gameweek-data
  [{:gw          1
    :player-data (merge-gw-player-data (read-statsbomb-data "statsbomb-player-data-gw1.edn")
                                       (:player-data prev-season-data)
                                       max-range-mins-total
                                       "both")
    :team-data   (merge-gw-team-data (read-statsbomb-data "statsbomb-team-data-gw1.edn")
                                     (:team-data prev-season-data)
                                     "both")}
   {:gw          2
    :player-data (merge-gw-player-data (read-statsbomb-data "statsbomb-player-data-gw2.edn")
                                       (:player-data prev-season-data)
                                       max-range-mins-total
                                       "both")
    :team-data   (merge-gw-team-data (read-statsbomb-data "statsbomb-team-data-gw2.edn")
                                     (:team-data prev-season-data)
                                     "both")}
   {:gw          3
    :player-data (merge-gw-player-data (read-statsbomb-data "statsbomb-player-data-gw3.edn")
                                       (:player-data prev-season-data)
                                       max-range-mins-total
                                       "both")
    :team-data   (merge-gw-team-data (read-statsbomb-data "statsbomb-team-data-gw3.edn")
                                     (:team-data prev-season-data)
                                     "both")}
   {:gw          4
    :player-data (merge-gw-player-data (read-statsbomb-data "statsbomb-player-data-gw4.edn")
                                       (:player-data prev-season-data)
                                       max-range-mins-total
                                       "both")
    :team-data   (merge-gw-team-data (read-statsbomb-data "statsbomb-team-data-gw4.edn")
                                     (:team-data prev-season-data)
                                     "both")}
   {:gw          5
    :player-data (merge-gw-player-data (read-statsbomb-data "statsbomb-player-data-gw5.edn")
                                       (:player-data prev-season-data)
                                       max-range-mins-total
                                       "both")
    :team-data   (merge-gw-team-data (read-statsbomb-data "statsbomb-team-data-gw5.edn")
                                     (:team-data prev-season-data)
                                     "both")}
   {:gw          6
    :player-data (merge-gw-player-data (read-statsbomb-data "statsbomb-player-data-gw6.edn")
                                       (:player-data prev-season-data)
                                       max-range-mins-total
                                       "both")
    :team-data   (merge-gw-team-data (read-statsbomb-data "statsbomb-team-data-gw6.edn")
                                     (:team-data prev-season-data)
                                     "both")}
   {:gw          7
    :player-data (merge-gw-player-data (read-statsbomb-data "statsbomb-player-data-gw7.edn")
                                       (:player-data prev-season-data)
                                       max-range-mins-total
                                       "both")
    :team-data   (merge-gw-team-data (read-statsbomb-data "statsbomb-team-data-gw7.edn")
                                     (:team-data prev-season-data)
                                     "both")}
   {:gw          8
    :player-data (merge-gw-player-data (read-statsbomb-data "statsbomb-player-data-gw8.edn")
                                       (:player-data prev-season-data)
                                       max-range-mins-total
                                       "both")
    :team-data   (merge-gw-team-data (read-statsbomb-data "statsbomb-team-data-gw8.edn")
                                     (:team-data prev-season-data)
                                     "both")}
   {:gw          9
    :player-data (merge-gw-player-data (read-statsbomb-data "statsbomb-player-data-gw9.edn")
                                       (:player-data prev-season-data)
                                       max-range-mins-total
                                       "both")
    :team-data   (merge-gw-team-data (read-statsbomb-data "statsbomb-team-data-gw9.edn")
                                     (:team-data prev-season-data)
                                     "both")}
   {:gw          10
    :player-data (merge-gw-player-data (read-statsbomb-data "statsbomb-player-data-gw10.edn")
                                       (:player-data prev-season-data)
                                       max-range-mins-total
                                       "both")
    :team-data   (merge-gw-team-data (read-statsbomb-data "statsbomb-team-data-gw10.edn")
                                     (:team-data prev-season-data)
                                     "both")}
   {:gw          11
    :player-data (merge-gw-player-data (read-statsbomb-data "statsbomb-player-data-gw11.edn")
                                       (:player-data prev-season-data)
                                       max-range-mins-total
                                       "both")
    :team-data   (merge-gw-team-data (read-statsbomb-data "statsbomb-team-data-gw11.edn")
                                     (:team-data prev-season-data)
                                     "both")}
   {:gw          12
    :player-data (merge-gw-player-data (read-statsbomb-data "statsbomb-player-data-gw12.edn")
                                       (:player-data prev-season-data)
                                       max-range-mins-total
                                       "both")
    :team-data   (merge-gw-team-data (read-statsbomb-data "statsbomb-team-data-gw12.edn")
                                     (:team-data prev-season-data)
                                     "both")}
   {:gw          13
    :player-data (merge-gw-player-data (read-statsbomb-data "statsbomb-player-data-gw13.edn")
                                       (:player-data prev-season-data)
                                       max-range-mins-total
                                       "both")
    :team-data   (merge-gw-team-data (read-statsbomb-data "statsbomb-team-data-gw13.edn")
                                     (:team-data prev-season-data)
                                     "both")}
   {:gw          14
    :player-data (merge-gw-player-data (read-statsbomb-data "statsbomb-player-data-gw14.edn")
                                       (:player-data prev-season-data)
                                       max-range-mins-total
                                       "both")
    :team-data   (merge-gw-team-data (read-statsbomb-data "statsbomb-team-data-gw14.edn")
                                     (:team-data prev-season-data)
                                     "both")}
   {:gw          15
    :player-data (merge-gw-player-data (read-statsbomb-data "statsbomb-player-data-gw15.edn")
                                       (:player-data prev-season-data)
                                       max-range-mins-total
                                       "both")
    :team-data   (merge-gw-team-data (read-statsbomb-data "statsbomb-team-data-gw15.edn")
                                     (:team-data prev-season-data)
                                     "both")}
   {:gw          16
    :player-data (merge-gw-player-data (read-statsbomb-data "statsbomb-player-data-gw16.edn")
                                       (:player-data prev-season-data)
                                       max-range-mins-total
                                       "both")
    :team-data   (merge-gw-team-data (read-statsbomb-data "statsbomb-team-data-gw16.edn")
                                     (:team-data prev-season-data)
                                     "both")}
   {:gw          17
    :player-data (merge-gw-player-data (read-statsbomb-data "statsbomb-player-data-gw17.edn")
                                       (:player-data prev-season-data)
                                       max-range-mins-total
                                       "both")
    :team-data   (merge-gw-team-data (read-statsbomb-data "statsbomb-team-data-gw17.edn")
                                     (:team-data prev-season-data)
                                     "both")}
   {:gw          18
    :player-data (merge-gw-player-data (read-statsbomb-data "statsbomb-player-data-gw18.edn")
                                       (:player-data prev-season-data)
                                       max-range-mins-total
                                       "both")
    :team-data   (merge-gw-team-data (read-statsbomb-data "statsbomb-team-data-gw18.edn")
                                     (:team-data prev-season-data)
                                     "both")}
   {:gw          19
    :player-data (merge-gw-player-data (read-statsbomb-data "statsbomb-player-data-gw19.edn")
                                       (:player-data prev-season-data)
                                       max-range-mins-total
                                       "both"
                                       true)
    :team-data   (merge-gw-team-data (read-statsbomb-data "statsbomb-team-data-gw19.edn")
                                     (:team-data prev-season-data)
                                     "both"
                                     true)}])

(defn- calculate-xg-diff-rank [data key]
  (sort-by key
           (map (fn [team]
                  (assoc team key (- (:team_season_np_xg_pg team)
                                     (:team_season_np_xg_conceded_pg team))))
                (filter #(not (some #{(:team_opta_id %)} [45 57 91])) data))))

(def team-xg-diff-rank
  (let [xg-diff-rank-now (reverse (calculate-xg-diff-rank (:team-data (last gameweek-data)) :xg-diff))
        xg-diff-rank-prev (calculate-xg-diff-rank (:team-data prev-season-data) :xg-diff-prev)
        combined-ranks (map (fn [team-xg-diff]
                              (let [prev-xg-diff (first (filter #(= (:team_id team-xg-diff) (:team_id %))
                                                            xg-diff-rank-prev))]
                                (assoc team-xg-diff :xg-diff-prev (:xg-diff-prev prev-xg-diff)
                                                    :team_season_np_xg_pg_prev (:team_season_np_xg_pg prev-xg-diff)
                                                    :team_season_np_xg_conceded_pg_prev
                                                    (:team_season_np_xg_conceded_pg prev-xg-diff))))
                            xg-diff-rank-now)]
    (pprint/pprint xg-diff-rank-now)
    combined-ranks))

(defn home-away-data [gw]
  (let [is-latest-gw? (= gw (:gw (last gameweek-data)))]
    {:player-data-home (merge-gw-player-data (read-statsbomb-data (str "home_away_data/statsbomb-player-data-home-gw" gw ".edn"))
                                             (:player-data-home prev-season-data)
                                             max-range-mins-home-away
                                             "home"
                                             is-latest-gw?)
     :player-data-away (merge-gw-player-data (read-statsbomb-data (str "home_away_data/statsbomb-player-data-away-gw" gw ".edn"))
                                             (:player-data-away prev-season-data)
                                             max-range-mins-home-away
                                             "away"
                                             is-latest-gw?)
     :team-data-home   (merge-gw-team-data (read-statsbomb-data (str "home_away_data/statsbomb-team-data-home-gw" gw ".edn"))
                                           (:team-data-home prev-season-data)
                                           "home"
                                           is-latest-gw?)
     :team-data-away   (merge-gw-team-data (read-statsbomb-data (str "home_away_data/statsbomb-team-data-away-gw" gw ".edn"))
                                           (:team-data-away prev-season-data)
                                           "away"
                                           is-latest-gw?)}))

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
           {:gw       9
            :test?    false
            :fixtures (filter (fn [fixture]
                                (and (.after (:date fixture) #inst "2020-11-21")
                                     (.before (:date fixture) #inst "2020-11-22")))
                              fixtures)
            :blanks   [45 57 91]}
           {:gw       10
            :test?    false
            :fixtures (filter (fn [fixture]
                                (and (.after (:date fixture) #inst "2020-11-28")
                                     (.before (:date fixture) #inst "2020-11-29")))
                              fixtures)
            :blanks   [45 57 91]}
           {:gw       11
            :test?    false
            :fixtures (filter (fn [fixture]
                                (and (.after (:date fixture) #inst "2020-12-05")
                                     (.before (:date fixture) #inst "2020-12-06")))
                              fixtures)
            :blanks   [45 57 91]}
           {:gw       12
            :test?    false
            :fixtures (filter (fn [fixture]
                                (and (.after (:date fixture) #inst "2020-12-12")
                                     (.before (:date fixture) #inst "2020-12-14")))
                              fixtures)
            :blanks   [45 57 91]}
           {:gw       13
            :test?    false
            :fixtures (filter (fn [fixture]
                                (and (.after (:date fixture) #inst "2020-12-15")
                                     (.before (:date fixture) #inst "2020-12-17")))
                              fixtures)
            :blanks   [45 57 91]}
           {:gw       14
            :test?    false
            :fixtures (filter (fn [fixture]
                                (and (.after (:date fixture) #inst "2020-12-19")
                                     (.before (:date fixture) #inst "2020-12-20")))
                              fixtures)
            :blanks   [45 57 91]}
           {:gw       15
            :test?    false
            :fixtures (filter (fn [fixture]
                                (and (.after (:date fixture) #inst "2020-12-26")
                                     (.before (:date fixture) #inst "2020-12-27")))
                              fixtures)
            :blanks   [45 57 91]}
           {:gw       16
            :test?    false
            :fixtures (filter (fn [fixture]
                                (and (.after (:date fixture) #inst "2020-12-28")
                                     (.before (:date fixture) #inst "2020-12-31")))
                              fixtures)
            :blanks   [45 57 91]}
           {:gw       17
            :test?    false
            :fixtures (filter (fn [fixture]
                                (and (.after (:date fixture) #inst "2021-01-01")
                                     (.before (:date fixture) #inst "2021-01-05")))
                              fixtures)
            :blanks   [45 57 91]}
           {:gw       18
            :test?    false
            :fixtures (filter (fn [fixture]
                                (and (.after (:date fixture) #inst "2021-01-12")
                                     (.before (:date fixture) #inst "2021-01-15")))
                              fixtures)
            :blanks   [45 57 91 14 13 8 20 21 2 54 35]}
           {:gw       19
            :fixtures (filter (fn [fixture]
                                (and (.after (:date fixture) #inst "2021-01-15")
                                     (.before (:date fixture) #inst "2021-01-22")))
                              fixtures)
            :test? false
            :doubles [{:home-team-id 183
                       :away-team-id 41}
                      {:home-team-id 110
                       :away-team-id 666}
                      ;{:home-team-id 184
                      ; :away-team-id 118}
                      {:home-team-id 87
                       :away-team-id 754}
                      {:home-team-id 733
                       :away-team-id 97}
                      {:home-team-id 247
                       :away-team-id 152}
                      {:home-team-id 152
                       :away-team-id 168}]}
           {:gw       20
            :test?    false
            :fixtures (filter (fn [fixture]
                                (and (.after (:date fixture) #inst "2021-01-26")
                                     (.before (:date fixture) #inst "2021-01-29")))
                              fixtures)
            :blanks   [45 57 91]}
           {:gw       21
            :test?    false
            :fixtures (filter (fn [fixture]
                                (and (.after (:date fixture) #inst "2021-01-30")
                                     (.before (:date fixture) #inst "2021-02-01")))
                              fixtures)
            :blanks   [45 57 91]}
           {:gw       22
            :test?    false
            :fixtures (filter (fn [fixture]
                                (and (.after (:date fixture) #inst "2021-02-02")
                                     (.before (:date fixture) #inst "2021-02-04")))
                              fixtures)
            :blanks   [45 57 91]}
           {:gw       23
            :test?    false
            :fixtures (filter (fn [fixture]
                                (and (.after (:date fixture) #inst "2021-02-06")
                                     (.before (:date fixture) #inst "2021-02-07")))
                              fixtures)
            :blanks   [45 57 91]}
           {:gw       24
            :test?    false
            :fixtures (filter (fn [fixture]
                                (and (.after (:date fixture) #inst "2021-02-13")
                                     (.before (:date fixture) #inst "2021-02-14")))
                              fixtures)
            :blanks   [45 57 91]}
           {:gw       25
            :test?    false
            :fixtures (filter (fn [fixture]
                                (and (.after (:date fixture) #inst "2021-02-20")
                                     (.before (:date fixture) #inst "2021-02-21")))
                              fixtures)
            :blanks   [45 57 91]}
           ]))