(ns fpl.core-all-gws
  (:require [fpl.scoring :refer [rules]]
            [fpl.gw-data :as gw-data]
            [fpl.gw-xg :as gw-xg]
            [clojure.pprint :refer [pprint]]))

(def total-matches-played 29)
(def gw-trend-window 5)
(def ema-weighting 0.3)

(defn- find-matching-sb-player [player statsbomb-data]
  (first (filter #(or (= (:player_opta_id %) (:code player))
                      (= (:player_id player) (:player_id %)))
                 ;(= (:second_name player) (:player_last_name %))
                 ;(if (not (nil? (:name %)))
                 ;  (= (:second_name player) (last (clojure.string/split (:name %) #" ")))
                 ;  false))
                 statsbomb-data)))

(defn- check-number-players-each-position [players]
  (let [{goalkeepers :1 defenders :2 midfielders :3 forwards :4} (into {} (for [[k v] (group-by :element_type players)]
                                                                            [(keyword (str k)) (count v)])
                                                                       )
        ]
    (cond
      (< goalkeepers 2) "Not enough goalkeepers"
      (< defenders 5) "Not enough defenders"
      (< midfielders 5) "Not enough midfielders"
      (< forwards 3) "Not enough forwards"
      :else nil)))

(defn- merge-expected-values [
                              players
                              statsbomb-player-data
                              statsbomb-team-data
                              & {:keys [expected-pts-symbol double-gw-team] :or {expected-pts-symbol :expected_points
                                                                                 double-gw-team      nil}}]
  (map (fn [player]

         (let [matching-sb-player (find-matching-sb-player player statsbomb-player-data)
               {team-xg-conceded :team_season_np_xg_conceded_pg
                team-id          :team_id} (first (filter #(= (:team_id matching-sb-player) (:team_id %))
                                                          statsbomb-team-data))
               team-multiplier (if (= team-id double-gw-team)
                                 1.5
                                 1)
               {gsaa              :player_season_gsaa_90
                player-xg         :player_season_np_xg_90
                player-xga        :player_season_xa_90
                player-shots      :player_season_np_shots_90
                player-conversion :player_season_conversion_ratio} matching-sb-player]
           (if (not (nil? matching-sb-player))
             (cond
               (= (:element_type player) 1) (merge player {expected-pts-symbol (* ((:gk-scores rules) team-xg-conceded
                                                                                   gsaa)
                                                                                  team-multiplier)})
               (= (:element_type player) 2) (merge player {expected-pts-symbol (* ((:def-scores rules) player-xg player-xga
                                                                                   team-xg-conceded)
                                                                                  team-multiplier)
                                                           })
               (= (:element_type player) 3) (merge player {expected-pts-symbol (* ((:mid-scores rules) player-xg player-xga
                                                                                   team-xg-conceded player-shots player-conversion)
                                                                                  team-multiplier)
                                                           })
               (= (:element_type player) 4) (merge player {expected-pts-symbol (* ((:fwd-scores rules) player-xg player-xga
                                                                                   player-shots player-conversion)
                                                                                  team-multiplier)
                                                           }))
             ;(clojure.pprint/pprint player)
             )))
       players))

(def current-team
  [{:name "Ings" :player_opta_id 84939 :keep 0 :discard 0}
   {:name "Calvert-Lewin" :player_opta_id 177815 :keep 0 :discard 0}
   {:name "Jota" :player_opta_id 194634 :keep 0}
   {:name "Barnes" :player_opta_id 201666 :keep 0}
   {:name "Salah" :player_opta_id 118748 :keep 1 :discard 0}
   {:name "De Bruyne" :player_opta_id 61366 :keep 1}
   {:name "Cantwell" :player_opta_id 193111 :keep 0 :discard 0}
   {:name "Doherty" :player_opta_id 87835 :keep 0}
   {:name "Alexander-Arnold" :player_opta_id 169187 :keep 1 :discard 0}
   {:name "Saiss" :player_opta_id 107613 :keep 0 :discard 0}
   {:name "De Gea" :player_opta_id 51940 :keep 0 :discard 0}
   {:name "Perez" :player_opta_id 168580 :keep 0 :discard 0}
   {:name "Otamendi" :player_opta_id 57410 :keep 0 :discard 0}

   {:name "McCarthy" :player_opta_id 58376 :keep 1}
   {:name "Tanganga" :player_opta_id 199584 :keep 1 :discard 0}])

(def wanted-players
  [
   ;{:name "Aguero" :player_opta_id 37572 :keep 1}
   ;{:name "Salah" :player_opta_id 118748 :keep 1}
   ;{:name "Robertson" :player_opta_id 122798 :keep 1}
   ;{:name "Firmino" :player_opta_id 92217 :keep 1}
   ;{:name "Mousset" :player_opta_id 178304 :keep 1}

   ;{:name "McCarthy" :player_opta_id 58376 :keep 1}
   ;{:name "Hanley" :player_opta_id 83428 :keep 1}
   ;{:name "Douglas Luiz" :player_opta_id 230046 :keep 1}
   ])

(defn- find-in-curr-team [player]
  (first (filter #(= (:player_opta_id %) (:code player)) current-team)))

(defn- find-in-wanted-team [player]
  (first (filter #(= (:player_opta_id %) (:code player)) wanted-players)))

(defn ema3 [c a]
  (loop [ct (rest c) res [(first c)]]
    (if (= (count ct) 0)
      res
      (recur
        (rest ct)
        (into
          res
          [(+ (* a (first ct)) (* (- 1 a) (peek res)))])))))

(defn- calculate-ema [player]
  (let [keys [:gw9 :gw10 :gw11 :gw12 :gw13 :gw14 :gw15 :gw16 :gw17 :gw18 :gw19 :gw20 :gw21 :gw22 :gw23 :gw24 :gw25
              :gw26 :gw27 :gw28 :gw29]
        pts-per-week (filter #(not (nil? %)) (map #(% player) keys))]
    (last (ema3 pts-per-week ema-weighting))))

(defn- calculate-trend [player]
  (let [keys [:gw9 :gw10 :gw11 :gw12 :gw13 :gw14 :gw15 :gw16 :gw17 :gw18 :gw19 :gw20 :gw21 :gw22 :gw23 :gw24 :gw25
              :gw26 :gw27 :gw28 :gw29]
        pts-per-week (filter #(not (nil? %)) (map #(% player) keys))]
    (- (last pts-per-week) (first (take-last gw-trend-window pts-per-week)))))

(defn- build-gw-str [data]
  (let [gw-prefix (if (< (:gw data) 10)
                    "0"
                    "")]
    (str "gw" gw-prefix (:gw data))))

(defn- build-expected-pts-str [data]
  (str (build-gw-str data) "-expected-points"))

(defn fpl-players [fpl-data]
  ;(filter #(= (:web_name %) "Jiménez")
  (:elements fpl-data))

(defn- add-fpl-players-with-sb-id [fpl-data filtered-gw-data]
  (println "add-fpl-players-with-sb-id")
  (mapv (fn [player]
          (assoc player :player_id (:player_id (find-matching-sb-player player (:player-data (last filtered-gw-data))
                                                                        ))))
        (fpl-players fpl-data)))

(defn- add-player-expected-points-per-gw [filtered-gw-data fpl-players-with-sb-id]
  (mapv (fn [data]
          (filter #(not (nil? %))
                  (merge-expected-values fpl-players-with-sb-id
                                         (:player-data data)
                                         (mapv (fn [team]
                                                 (assoc team
                                                   :team_season_np_xg_conceded_pg
                                                   [(:team_season_np_xg_conceded_pg team)]))
                                               (:team-data data))
                                         :expected-pts-symbol (keyword (build-gw-str data)))))
        filtered-gw-data))

(defn- join-expected-points [player-expected-points-per-gw]
  (println "join-expected-points")
  (reduce (fn [gw-list1, gw-list2]
            (mapv
              (fn [player]
                (merge player (first (filter #(= (:code %) (:code player)) gw-list2))))
              gw-list1))
          (reverse player-expected-points-per-gw)))

;(clojure.pprint/pprint (first (filter #(= (:web_name %) "Jiménez")  player-expected-points-joined)))

(defn- add-players-ema [player-expected-points-joined]
  (println "add-players-ema")
  (mapv (fn [player]
          (assoc player :ema (calculate-ema player)
                        :trend (calculate-trend player)))
        player-expected-points-joined))

(defn- add-player-expected-points-future-gws [players-ema filtered-gw-data latest-fixture fixtures]
  (println "add-player-expected-points-future-gws")
  (mapv (fn [data]
          (filter #(not (nil? %))
                  (merge-expected-values players-ema
                                         (:player-data data)
                                         (:team-data data)
                                         :expected-pts-symbol (keyword (build-expected-pts-str data)))))
        (gw-xg/expected-gw-points filtered-gw-data latest-fixture fixtures)))

(defn- join-player-expected-points-future-gws [player-expected-points-future-gws]
  (println "join-player-expected-points-future-gws")
  (reduce (fn [gw-list1 gw-list2]
            (mapv
              (fn [player]
                (merge player (first (filter #(= (:code %) (:code player)) gw-list2))))
              gw-list1))
          player-expected-points-future-gws))

(defn- add-player-expected-points-future-gws-total [player-expected-points-future-gws-joined fixtures]
  (println "add-player-expected-points-future-gws-total")
  (mapv (fn [player]
          (let [expected-points-total (reduce (fn [total fixture]
                                                (let [expected-pts-symbol (keyword (build-expected-pts-str {:gw fixture}))
                                                      expected-pts (expected-pts-symbol player)]
                                                  (+ total (if (nil? expected-pts)
                                                             0
                                                             expected-pts))))
                                              0
                                              fixtures)]
            (assoc player :expected-points-total expected-points-total)))
        player-expected-points-future-gws-joined))

(defn calculate-expected-values [fpl-data to-gw latest-fixture fixtures]
  (let [filtered-gw-data (filter #(<= (:gw %) to-gw) gw-data/gameweek-data)
        fpl-players-with-sb-id (add-fpl-players-with-sb-id fpl-data filtered-gw-data)
        player-expected-points-per-gw (add-player-expected-points-per-gw filtered-gw-data fpl-players-with-sb-id)
        player-expected-points-joined (join-expected-points player-expected-points-per-gw)
        players-ema (add-players-ema player-expected-points-joined)
        player-expected-points-future-gws (add-player-expected-points-future-gws players-ema filtered-gw-data latest-fixture
                                                                                 fixtures)
        player-expected-points-future-gws-joined (join-player-expected-points-future-gws player-expected-points-future-gws)
        player-expected-points-future-gws-total (add-player-expected-points-future-gws-total
                                                  player-expected-points-future-gws-joined fixtures)
        fpl-players-with-curr-team (map (fn [player]
                                          (if-let [player-in-curr-team (find-in-curr-team player)]
                                            (assoc player :current_team 1 :keep (:keep player-in-curr-team)
                                                          :discard (:discard player-in-curr-team))
                                            (if-let [player-in-wanted-team (find-in-wanted-team player)]
                                              (assoc player :keep 1 :current_team 0)
                                              (assoc player :current_team 0 :keep 0))))
                                        player-expected-points-future-gws-total)
        check-number-players-each-position (check-number-players-each-position fpl-players-with-curr-team)]
    (if (nil? check-number-players-each-position)
      (let [players (map (fn [player]
                           (let [plyr (select-keys player [:web_name :now_cost :current_team :code :element_type
                                                           :total_points :minutes
                                                           :current_team :expected_points :team_code :keep :gw9 :gw10 :gw11 :gw12 :gw13
                                                           :gw14 :gw15 :gw16 :gw17 :gw18 :gw19 :gw20 :gw21 :gw22 :gw23 :gw24 :gw25
                                                           :gw26 :gw27 :gw28 :gw29
                                                           :discard :ema :trend :gw30-expected-points
                                                           :gw31-expected-points
                                                           :gw36-expected-points
                                                           :gw37-expected-points :gw38-expected-points
                                                           :expected-points-total])]
                             (assoc plyr :expected_points_per_90
                                         ((keyword (str "gw" latest-fixture)) plyr))))
                         fpl-players-with-curr-team)]
        (merge (:body fpl-data) {:elements players :total_matches_played total-matches-played}))
      (println check-number-players-each-position))))