(ns fpl.gw-xg
  (:require [fpl.gw-data :as gw-data]))

(defn- team-xg-conceded [team-xg-conceded opposing-team-xg]
  (+ team-xg-conceded (- opposing-team-xg 1)))

(defn- player-xg [player-xg opposing-team-xg-conceded]
  (let [xg-val (+ player-xg (- opposing-team-xg-conceded 1))]
    (if (< xg-val 0)
      0
      xg-val)))

; THIS DOESN'T WORK FOR DOUBLE GAMEWEEKS!!!
(defn- gw-player-xg [players fixtures blanks]
  (mapv (fn [player]
          (let [player-team-id (:team_id player)
                {home-team-id :home_team_id
                 away-team-id :away_team_id} (first (filter #(or (= player-team-id (:home_team_id %))
                                                                 (= player-team-id (:away_team_id %)))
                                                            fixtures))
                opposing-team-id (if (= home-team-id player-team-id)
                                   away-team-id
                                   home-team-id)
                player-data (if (= home-team-id player-team-id)
                              (first (filter #(= (:player_id player) (:player_id %)) (:player-data-home
                                                                                  gw-data/home-away-data)))
                              (first (filter #(= (:player_id player) (:player_id %)) (:player-data-away
                                                                                  gw-data/home-away-data))))
                ;log (if (nil? (:player_season_np_xg_90 player-data))
                ;      (do (clojure.pprint/pprint player)
                ;          (clojure.pprint/pprint home-team-id)
                ;          (clojure.pprint/pprint away-team-id)))
                opposing-team (if (= home-team-id player-team-id)
                                (first (filter #(= opposing-team-id (:team_id %)) (:team-data-away
                                                                                    gw-data/home-away-data)))
                                (first (filter #(= opposing-team-id (:team_id %)) (:team-data-home
                                                                                    gw-data/home-away-data))))
                player-xg-val (if (nil? player-data)
                                0
                                (player-xg (:player_season_np_xg_90 player-data)
                                           (:team_season_np_xg_conceded_pg opposing-team)))
                has-blank-gw? (some #(= player-team-id %) blanks)]
            (if (true? has-blank-gw?)
              (assoc player-data :player_season_xa_90 0
                                 :player_season_np_xg_90 0
                                 :player_season_gsaa_90 0)
              (assoc player-data :player_season_np_xg_90 player-xg-val))))
        players))

(defn- gw-team-xg [teams fixtures blanks]
  (mapv (fn [team]
          (let [team-id (:team_id team)
                {home-team-id :home_team_id
                 away-team-id :away_team_id} (first (filter #(or (= team-id (:home_team_id %))
                                                                 (= team-id (:away_team_id %)))
                                                            fixtures))
                opposing-team-id (if (= home-team-id team-id)
                                   away-team-id
                                   home-team-id)
                team-data (if (= home-team-id team-id)
                              (first (filter #(= team-id (:team_id %)) (:team-data-home
                                                                                       gw-data/home-away-data)))
                              (first (filter #(= team-id (:team_id %)) (:team-data-away
                                                                                       gw-data/home-away-data))))
                opposing-team (if (= home-team-id team-id)
                                (first (filter #(= opposing-team-id (:team_id %)) (:team-data-away
                                                                                    gw-data/home-away-data)))
                                (first (filter #(= opposing-team-id (:team_id %)) (:team-data-home
                                                                                    gw-data/home-away-data))))
                team-xg-val (if (nil? team-data)
                              0
                              (team-xg-conceded (:team_season_np_xg_conceded_pg team-data)
                                                  (:team_season_np_xg_pg
                                                    opposing-team)))
                has-blank-gw? (some #(= team-id %) blanks)
                ;log (if (true? has-blank-gw?)
                ;      (do (clojure.pprint/pprint team-data)
                ;          (clojure.pprint/pprint home-team-id)
                ;          (clojure.pprint/pprint away-team-id)))
                ]
            (if (true? has-blank-gw?)
              (assoc team-data :team_season_np_xg_conceded_pg 0)
              (assoc team-data :team_season_np_xg_conceded_pg team-xg-val))))
        teams))

(def expected-gw-points
  (let [most-recent-data (last gw-data/gameweek-data)
        player-data (:player-data most-recent-data)
        team-data (:team-data most-recent-data)]
    (mapv (fn [{:keys [gw fixtures blanks]}]
            {:gw gw
             :player-data (gw-player-xg player-data fixtures blanks)
             :team-data (gw-team-xg team-data fixtures blanks)})
          gw-data/fixture-data)))

;(clojure.pprint/pprint (first (filter #(= (:name %) "Andreas Pereira")  (:player-data (first expected-gw-points)))))
;(clojure.pprint/pprint (first (filter #(= (:team_id %) 754)  (:team-data (first expected-gw-points)))))