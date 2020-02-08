(ns fpl.core-all-gws
  (:require [clj-http.client :as client]
            [fpl.scoring :refer [rules]]
            [clojure.data.json :as json]
            [ring.adapter.jetty :refer [run-jetty]]
            [fpl.gw-data :as gw-data]
            [fpl.gw-xg :as gw-xg]))

(def total-matches-played 25)
(def minimum-minutes 1170)
(def gw-trend-window 5)
(def ema-weighting 0.3)

(defn- get-fpl-data []
  (client/get "https://fantasy.premierleague.com/api/bootstrap-static/" {:as :json}))

(defn- find-matching-sb-player [player statsbomb-data]
  (first (filter #(or (= (:player_opta_id %) (:code player))
                      (= (:player_id player) (:player_id %))
                      (= (:second_name player) (:player_last_name %))
                      (if (not (nil? (:name %)))
                        (= (:second_name player) (last (clojure.string/split (:name %) #" ")))
                        false))
                 statsbomb-data)))

(defn- check-number-players-each-position [players]
  (let [{goalkeepers :1 defenders :2 midfielders :3 forwards :4}  (into {} (for [[k v] (group-by :element_type players)]
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
                                                                                 double-gw-team nil}}]
  (map (fn [player]

         (let [matching-sb-player (find-matching-sb-player player statsbomb-player-data)
               {team-xg-conceded :team_season_np_xg_conceded_pg
                team-id :team_id} (first (filter #(= (:team_id matching-sb-player) (:team_id %))
                                                 statsbomb-team-data))
               team-multiplier (if (= team-id double-gw-team)
                                 1.5
                                 1)
               {gsaa :player_season_gsaa_90
                player-xg :player_season_np_xg_90
                player-xga :player_season_xa_90
                player-shots :player_season_np_shots_90
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
               ;(clojure.pprint/pprint expected-pts-symbol)
              )))
       players))

(def current-team
  [{:name "Firmino" :player_opta_id 92217 :keep 0 :discard 0}
   {:name "Calvert-Lewin" :player_opta_id 177815 :keep 0 :discard 0}
   {:name "Ings" :player_opta_id 84939 :keep 0 :discard 0}
   {:name "Pereira" :player_opta_id 156689 :keep 0 :discard 0}
   {:name "Salah" :player_opta_id 118748 :keep 0 :discard 0}
   {:name "De Bruyne" :player_opta_id 61366 :keep 0 :discard 0}
   {:name "Cantwell" :player_opta_id 193111 :keep 0 :discard 0}
   {:name "Söyüncü" :player_opta_id 218031 :keep 0 :discard 0}
   {:name "Alexander-Arnold" :player_opta_id 169187 :keep 0 :discard 0}
   {:name "Tanganga" :player_opta_id 199584 :keep 1 :discard 0}
   {:name "Lundstram" :player_opta_id 153723 :keep 0 :discard 1}
   {:name "Ryan" :player_opta_id 131897 :keep 0 :discard 0}
   {:name "Schmeichel" :player_opta_id 17745 :keep 0 :discard 0}
   {:name "Perez" :player_opta_id 168580 :keep 0 :discard 0}
   {:name "Maguire" :player_opta_id 95658 :keep 0 :discard 0}])

(def wanted-players
  [
   ;{:name "Aguero" :player_opta_id 37572 :keep 1}
   ;{:name "Salah" :player_opta_id 118748 :keep 1}
   ;{:name "Robertson" :player_opta_id 122798 :keep 1}
   ;{:name "Firmino" :player_opta_id 92217 :keep 1}
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
  (let [keys [:gw09 :gw10 :gw11 :gw12 :gw13 :gw14 :gw15 :gw16 :gw17 :gw18 :gw19 :gw20 :gw21 :gw22 :gw23 :gw24 :gw25]
        pts-per-week (filter #(not (nil? %)) (map #(% player) keys))]
    (last (ema3 pts-per-week ema-weighting))))

(defn- calculate-trend [player]
  (let [keys [:gw09 :gw10 :gw11 :gw12 :gw13 :gw14 :gw15 :gw16 :gw17 :gw18 :gw19 :gw20 :gw21 :gw22 :gw23 :gw24 :gw25]
        pts-per-week (filter #(not (nil? %)) (map #(% player) keys))]
    (- (last pts-per-week) (first (take-last gw-trend-window pts-per-week)))))

(def fpl-data
  (get-fpl-data))

(def fpl-players
  (-> fpl-data
      :body
      :elements))

(def fpl-players-with-sb-id
  (mapv (fn [player]
          (assoc player :player_id (:player_id (find-matching-sb-player player (:player-data (last
                                                                                       gw-data/gameweek-data))))))
        fpl-players))

(def player-expected-points-per-gw
  (mapv (fn [data]
          (filter #(not (nil? %))
                  (merge-expected-values fpl-players-with-sb-id
                                 (:player-data data)
                                 (:team-data data)
                                 :expected-pts-symbol (keyword (str "gw"(:gw data))))))
        gw-data/gameweek-data))

(def player-expected-points-joined
  (reduce (fn [gw-list1, gw-list2]
            (mapv
              (fn [player]
                (merge player (first (filter #(= (:code %) (:code player)) gw-list2))))
              gw-list1))
          (reverse player-expected-points-per-gw)))

(def players-with-ema
  (mapv (fn [player]
          (assoc player :ema (calculate-ema player)
                        :trend (calculate-trend player)))
        player-expected-points-joined))

(def player-expected-points-future-gws
  (mapv (fn [data]
          (filter #(not (nil? %))
            (merge-expected-values players-with-ema
                                   (:player-data data)
                                   (:team-data data)
                                 :expected-pts-symbol (keyword (str "gw" (:gw data) "-expected-points")))))
        gw-xg/expected-gw-points))

(def player-expected-points-future-gws-joined
  (reduce (fn [gw-list1, gw-list2]
            (mapv
              (fn [player]
                (merge player (first (filter #(= (:code %) (:code player)) gw-list2))))
              gw-list1))
          player-expected-points-future-gws))

(def player-expected-points-future-gws-total
  (mapv (fn [player]
          (assoc player :expected-points-total (+ (if (nil? (:gw26-expected-points player))
                                                    0
                                                    (:gw26-expected-points player))
                                                  (if (nil? (:gw27-expected-points player))
                                                    0
                                                    (:gw27-expected-points player))
                                                  (if (nil? (:gw28-expected-points player))
                                                    0
                                                    (:gw28-expected-points player)))))
        player-expected-points-future-gws-joined))

(def fpl-players-per-mins
  (filter
    (fn [player]
      (or
        (and (>= (:minutes player) minimum-minutes)
             (every? #(not (= (:web_name player) %)) [;"Jesus"
                                                      ;"El Ghazi"
                                                      "McGoldrick"
                                                      ;"Bernardo Silva"
                                                      ;"Haller"
                                                      ;"Mané"
                                                      ;"Sterling"
                                                      ;"De Bruyne"
                                                      ]))
        (some #(= (:web_name player) %) ["Salah"
                                         "Agüero"
                                         "Tanganga"
                                         ;"Noble"
                                         ;"Felipe Anderson"
                                         ;"Snodgrass"
                                         ;"Fornals"
                                         ;"Rice"
                                         ;"Lanzini"
                                         ])))
    player-expected-points-future-gws-total))

(defn- calculate-expected-values []
  (let [fpl-players-with-curr-team (map (fn [player]
                                          (if-let [player-in-curr-team (find-in-curr-team player)]
                                            (assoc player :current_team 1 :keep (:keep player-in-curr-team)
                                                          :discard (:discard  player-in-curr-team))
                                            (if-let [player-in-wanted-team (find-in-wanted-team player)]
                                              (assoc player :keep 1 :current_team 0)
                                              (assoc player :current_team 0 :keep 0))))
                                        fpl-players-per-mins)
        check-number-players-each-position (check-number-players-each-position fpl-players-with-curr-team)]
    (if (nil? check-number-players-each-position)
      (let [players (map (fn [player]
                           (let [plyr (select-keys player [:web_name :now_cost :current_team :code :element_type
                                                     :total_points :minutes
                                          :current_team :expected_points :team_code :keep :gw09 :gw10 :gw11 :gw12 :gw13
                                          :gw14 :gw15 :gw16 :gw17 :gw18 :gw19 :gw20 :gw21 :gw22 :gw23 :gw24 :gw25
                                          :discard :ema :trend :gw26-expected-points :gw27-expected-points
                                                           :gw28-expected-points :expected-points-total])]
                             (assoc plyr :expected_points_per_90 (:gw25 plyr))))
                         fpl-players-with-curr-team)]
        (json/write-str (merge (:body fpl-data) {:elements players :total_matches_played total-matches-played})))
      (println check-number-players-each-position))))

(defn handler [request]
  {:status 200
   :headers {"Content-Type" "application/json"}
   :body (calculate-expected-values)})

(defn -main [] (run-jetty handler {:port 3000}))