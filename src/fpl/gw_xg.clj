(ns fpl.gw-xg
  (:require [fpl.gw-data :as gw-data]))

(defn- team-xg-conceded [team-xg-conceded opposing-team-xg average-xg]
  (* (/ opposing-team-xg average-xg) team-xg-conceded))

(defn- player-xg [player-xg opposing-team-xg-conceded average-xg-conceded]
  (let [xg-val (* (/ opposing-team-xg-conceded average-xg-conceded) player-xg)]
    (if (< xg-val 0)
      0
      xg-val)))

(defn- collect-player-data [player player-team-id matches home-away-data]
  (mapv (fn [{:keys [home-team-id away-team-id]}]
          (let [player-data (if (= home-team-id player-team-id)
                              (first (filter #(= (:player_id player) (:player_id %)) (:player-data-home
                                                                                       home-away-data)))
                              (first (filter #(= (:player_id player) (:player_id %)) (:player-data-away
                                                                                       home-away-data
                                                                                       ))))
                ;log (if (nil? (:player_season_np_xg_90 player-data))
                ;      (do (clojure.pprint/pprint player)
                ;          (clojure.pprint/pprint home-team-id)
                ;          (clojure.pprint/pprint away-team-id)))
                opposing-team (if (= home-team-id player-team-id)
                                (first (filter #(= away-team-id (:team_id %)) (:team-data-away
                                                                                home-away-data)))
                                (first (filter #(= home-team-id (:team_id %)) (:team-data-home
                                                                                home-away-data))))
                average-xg-conceded (/ (reduce #(+ %1 %2)
                                               (map #(:team_season_np_xg_conceded_pg %)
                                                    (filter #(not (= (:team_id %) player-team-id))
                                                            (if (= home-team-id player-team-id)
                                                              (:team-data-away home-away-data)
                                                              (:team-data-home home-away-data)))))
                                       19)
                ;log (if (= (:name player) "Mohamed Salah")
                ;      (println average-xg-conceded))
                ]
            {:player-data         player-data
             :opposing-team       opposing-team
             :average-xg-conceded average-xg-conceded}))
        matches))

(defn- collect-team-data [team team-id matches home-away-data]
  (mapv (fn [{:keys [home-team-id away-team-id]}]
          (let [team-data (if (= home-team-id team-id)
                            (first (filter #(= team-id (:team_id %)) (:team-data-home
                                                                       home-away-data)))
                            (first (filter #(= team-id (:team_id %)) (:team-data-away
                                                                       home-away-data))))
                opposing-team (if (= home-team-id team-id)
                                (first (filter #(= away-team-id (:team_id %)) (:team-data-away
                                                                                home-away-data)))
                                (first (filter #(= home-team-id (:team_id %)) (:team-data-home
                                                                                home-away-data))))
                average-xg (/ (reduce #(+ %1 %2)
                                      (map #(:team_season_np_xg_pg %)
                                           (filter (fn [other-team]
                                                     (not (= (:team_id other-team) (:team_id opposing-team))))
                                                   (if (= home-team-id (:team_id opposing-team))
                                                     (:team-data-home home-away-data)
                                                     (:team-data-away home-away-data)))))
                              19)
                ;log (when (= (:team_name team) "Manchester United")
                ;      (println "man u xg:")
                ;      (println average-xg)
                ;      (println "\n"))
                ]
            {:team-data     team-data
             :opposing-team opposing-team
             :average-xg    average-xg}))
        matches))

(defn- calculate-xa [player-dataA player-dataB]
  ; some players may have home data but not away data or vice versa
  (cond (nil? player-dataA) (* (:player_season_xa_90 player-dataB) 2)
        (nil? player-dataB) (* (:player_season_xa_90 player-dataA) 2)
        :else (+ (:player_season_xa_90 player-dataA)
                 (:player_season_xa_90 player-dataB))))

(defn- calculate-gsaa [player-dataA player-dataB]
  ; some players may have home data but not away data or vice versa
  (cond (nil? player-dataA) (* (:player_season_gsaa_90 player-dataB) 2)
        (nil? player-dataB) (* (:player_season_gsaa_90 player-dataA) 2)
        :else (+ (:player_season_gsaa_90 player-dataA)
                 (:player_season_gsaa_90 player-dataB))))

(defn- gw-player-xg [players fixtures blanks doubles home-away-data]
  (mapv (fn [player]
          (let [player-team-id (:team_id player)
                match [(clojure.set/rename-keys
                         (select-keys (first (filter #(or (= player-team-id (:home_team_id %))
                                                          (= player-team-id (:away_team_id %)))
                                                     fixtures))
                                      [:home_team_id :away_team_id])
                         {:home_team_id :home-team-id
                          :away_team_id :away-team-id})]
                all-matches (concat match
                                    (filter (fn [match] (if (or (= (:home-team-id match) player-team-id)
                                                                (= (:away-team-id match) player-team-id))
                                                          true
                                                          false))
                                            doubles))
                player-datas (collect-player-data player player-team-id all-matches home-away-data)
                ;opposing-team-ids (concat [(if (= home-team-id player-team-id)
                ;                             away-team-id
                ;                             home-team-id)]
                ;                          (filter (fn [{:keys [home-team-id away-team-id]}]
                ;                                    (if (= home-team-id player-team-id)
                ;                                      away-team-id
                ;                                      home-team-id))
                ;                                  doubles))
                ;player-data (if (= home-team-id player-team-id)
                ;              (first (filter #(= (:player_id player) (:player_id %)) (:player-data-home
                ;                                                                       gw-data/home-away-data)))
                ;              (first (filter #(= (:player_id player) (:player_id %)) (:player-data-away
                ;                                                                       gw-data/home-away-data))))
                ;;log (if (nil? (:player_season_np_xg_90 player-data))
                ;;      (do (clojure.pprint/pprint player)
                ;;          (clojure.pprint/pprint home-team-id)
                ;;          (clojure.pprint/pprint away-team-id)))
                ;opposing-team (if (= home-team-id player-team-id)
                ;                (first (filter #(= opposing-team-id (:team_id %)) (:team-data-away
                ;                                                                    gw-data/home-away-data)))
                ;                (first (filter #(= opposing-team-id (:team_id %)) (:team-data-home
                ;                                                                    gw-data/home-away-data))))
                ;average-xg-conceded (/ (reduce #(+ %1 %2)
                ;                               (map #(:team_season_np_xg_conceded_pg %)
                ;                                    (filter #(not (= (:team_id %) player-team-id))
                ;                                            (if (= home-team-id player-team-id)
                ;                                              (:team-data-away gw-data/home-away-data)
                ;                                              (:team-data-home gw-data/home-away-data)))))
                ;                       19)
                player-xg-val (reduce #(+ %1 %2)
                                      (mapv (fn [{:keys [player-data opposing-team average-xg-conceded]}]
                                              ;(when (= (:name player-data) "Mohamed Salah")
                                              ;  (println average-xg-conceded)
                                              ;  (println (:player_season_np_xg_90 player-data))
                                              ;  (println (:team_season_np_xg_conceded_pg opposing-team)))
                                              (if (nil? player-data)
                                                0
                                                (player-xg (:player_season_np_xg_90 player-data)
                                                           (:team_season_np_xg_conceded_pg opposing-team)
                                                           average-xg-conceded)))
                                            player-datas))
                ;log (when (= (:name player) "Mohamed Salah")
                ;      (println player-xg-val))
                has-blank-gw? (some #(= player-team-id %) blanks)]
            (cond
              (true? has-blank-gw?) (assoc (:player-data (first player-datas)) :player_season_xa_90 0
                                                                               :player_season_np_xg_90 0
                                                                               :player_season_gsaa_90 0)
              (> (count player-datas) 1) (reduce (fn [player-dataA player-dataB]
                                                   (assoc player-dataA
                                                     :player_season_np_xg_90 player-xg-val
                                                     :player_season_xa_90 (calculate-xa player-dataA player-dataB)
                                                     :player_season_gsaa_90 (calculate-gsaa player-dataA player-dataB)))
                                                 (mapv #(:player-data %) player-datas))
              :else (assoc (:player-data (first player-datas)) :player_season_np_xg_90 player-xg-val))))
        players))

(defn- gw-team-xg [teams fixtures blanks doubles home-away-data]
  (mapv (fn [team]
          (let [team-id (:team_id team)
                ;{home-team-id :home_team_id
                ; away-team-id :away_team_id} (first (filter #(or (= team-id (:home_team_id %))
                ;                                                 (= team-id (:away_team_id %)))
                ;                                            fixtures))
                matches (concat [(clojure.set/rename-keys
                                   (select-keys (first (filter #(or (= team-id (:home_team_id %))
                                                                    (= team-id (:away_team_id %)))
                                                               fixtures))
                                                [:home_team_id :away_team_id])
                                   {:home_team_id :home-team-id
                                    :away_team_id :away-team-id})]
                                (filter (fn [match] (if (or (= (:home-team-id match) team-id)
                                                            (= (:away-team-id match) team-id))
                                                      true
                                                      false))
                                        doubles))
                ;opposing-team-id (if (= home-team-id team-id)
                ;                   away-team-id
                ;                   home-team-id)
                ;team-data (if (= home-team-id team-id)
                ;            (first (filter #(= team-id (:team_id %)) (:team-data-home
                ;                                                       gw-data/home-away-data)))
                ;            (first (filter #(= team-id (:team_id %)) (:team-data-away
                ;                                                       gw-data/home-away-data))))
                ;opposing-team (if (= home-team-id team-id)
                ;                (first (filter #(= opposing-team-id (:team_id %)) (:team-data-away
                ;                                                                    gw-data/home-away-data)))
                ;                (first (filter #(= opposing-team-id (:team_id %)) (:team-data-home
                ;                                                                    gw-data/home-away-data))))
                ;average-xg (/ (reduce #(+ %1 %2)
                ;                      (map #(:team_season_np_xg_pg %)
                ;                           (filter #(not (= (:team_id %) team-id))
                ;                                   (if (= home-team-id team-id)
                ;                                     (:team-data-away gw-data/home-away-data)
                ;                                     (:team-data-home gw-data/home-away-data)))))
                ;              19)
                team-datas (collect-team-data team team-id matches home-away-data)
                team-xg-vals (mapv (fn [{:keys [team-data opposing-team average-xg]}]
                                     ;(when (= (:team_name team) "Arsenal")
                                     ;  ;(println (:team_season_np_xg_conceded_pg team-data))
                                     ;  ;(println (:team_season_np_xg_pg opposing-team))
                                     ;  (println average-xg))
                                     (if (nil? team-data)
                                       0
                                       (team-xg-conceded (:team_season_np_xg_conceded_pg team-data)
                                                         (:team_season_np_xg_pg opposing-team)
                                                         average-xg)))
                                   team-datas)
                ;log (when (= (:team_name team) "Crystal Palace")
                ;      (println "\n")
                ;      (println team-xg-vals))
                has-blank-gw? (some #(= team-id %) blanks)
                ;log (if (true? has-blank-gw?)
                ;      (do (clojure.pprint/pprint team-data)
                ;          (clojure.pprint/pprint home-team-id)
                ;          (clojure.pprint/pprint away-team-id)))
                ]
            (if (true? has-blank-gw?)
              (assoc (:team-data (first team-datas)) :team_season_np_xg_conceded_pg [0])
              (assoc (:team-data (first team-datas)) :team_season_np_xg_conceded_pg team-xg-vals))))
        teams))

(defn expected-gw-points [filtered-gw-data latest-fixture fixtures-to-retrieve]
  (let [most-recent-data (last filtered-gw-data)
        player-data (:player-data most-recent-data)
        team-data (:team-data most-recent-data)
        home-away-data (gw-data/home-away-data latest-fixture)]
    (mapv (fn [{:keys [gw fixtures blanks doubles]}]
            {:gw          gw
             :player-data (gw-player-xg player-data fixtures blanks doubles home-away-data)
             :team-data   (gw-team-xg team-data fixtures blanks doubles home-away-data)})
          (gw-data/fixture-data fixtures-to-retrieve))))


;(clojure.pprint/pprint (first (filter #(= (:name %) "Danny Ings") (:player-data (first expected-gw-points)))))
;(clojure.pprint/pprint (first (filter #(= (:team_id %) 733) (:team-data (first expected-gw-points)))))