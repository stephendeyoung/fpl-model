(ns fpl.core-with-curr-team
  (:require [clj-http.client :as client]
            [fpl.scoring :refer [rules]]
            [clojure.edn :as edn]
            [clojure.data.json :as json]
            [ring.adapter.jetty :refer [run-jetty]]))

(defn- read-statsbomb-data [filename]
  (edn/read-string (slurp (str "resources/" filename))))

(defn- get-fpl-data []
  (client/get "https://fantasy.premierleague.com/api/bootstrap-static/" {:as :json}))

(defn- find-matching-sb-player [player statsbomb-data]
  (first (filter #(= (:player_opta_id %) (:code player)) statsbomb-data)))

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

(defn- merge-expected-values [players statsbomb-player-data statsbomb-team-data double-gw-team]
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

           (cond
             (= (:element_type player) 1) (merge player {:expected_value (* ((:gk-scores rules) team-xg-conceded
                                                                             gsaa)
                                                                            team-multiplier)})
             (= (:element_type player) 2) (merge player {:expected_value (* ((:def-scores rules) player-xg player-xga
                                                                          team-xg-conceded)
                                                                            team-multiplier)
                                                         })
             (= (:element_type player) 3) (merge player {:expected_value (* ((:mid-scores rules) player-xg player-xga
                                                                          team-xg-conceded player-shots player-conversion)
                                                                            team-multiplier)
                                                         })
             (= (:element_type player) 4) (merge player {:expected_value (* ((:fwd-scores rules) player-xg player-xga
                                                                           player-shots player-conversion)
                                                                            team-multiplier)
                                                         }))))
       players))

(def current-team
  [{:name "Firmino" :player_opta_id 92217 :keep 0 :discard 0}
   {:name "Abraham" :player_opta_id 173879 :keep 0 :discard 1}
   {:name "Ings" :player_opta_id 84939 :keep 0 :discard 0}
   {:name "Pereira" :player_opta_id 156689 :keep 0 :discard 0}
   {:name "Salah" :player_opta_id 118748 :keep 0 :discard 0}
   {:name "De Bruyne" :player_opta_id 61366 :keep 0 :discard 0}
   {:name "Cantwell" :player_opta_id 193111 :keep 0 :discard 0}
   {:name "Söyüncü" :player_opta_id 218031 :keep 0 :discard 0}
   {:name "Alexander-Arnold" :player_opta_id 169187 :keep 0 :discard 0}
   {:name "Tanganga" :player_opta_id 199584 :keep 1 :discard 0}
   {:name "Lundstram" :player_opta_id 153723 :keep 0 :discard 0}
   {:name "Ryan" :player_opta_id 131897 :keep 0 :discard 0}
   {:name "Schmeichel" :player_opta_id 17745 :keep 0 :discard 0}
   {:name "Perez" :player_opta_id 168580 :keep 0 :discard 0}
   {:name "Maguire" :player_opta_id 95658 :keep 0 :discard 0}])

(def wanted-players
  [
   ;{:name "Mané" :player_opta_id 110979 :keep 1}
   ;{:name "Salah" :player_opta_id 118748 :keep 1}
   ;{:name "Robertson" :player_opta_id 122798 :keep 1}
   ;{:name "Firmino" :player_opta_id 92217 :keep 1}
   ])

(defn- find-in-curr-team [player]
  (first (filter #(= (:player_opta_id %) (:code player)) current-team)))

(defn- find-in-wanted-team [player]
  (first (filter #(= (:player_opta_id %) (:code player)) wanted-players)))

(defn- calculate-expected-values []
  (let [fpl-data (get-fpl-data)
        fpl-players (-> fpl-data
                        :body
                        :elements)
        statsbomb-player-data (read-statsbomb-data "statsbomb-player-data-gw23.edn")
        statsbomb-team-data (read-statsbomb-data "statsbomb-team-data-gw23.edn")
        fpl-players-per-mins (filter
                               (fn [player]
                                 (if-let [matching-sb-player (find-matching-sb-player player statsbomb-player-data)]
                                   (or
                                     (and (>= (:minutes matching-sb-player) 1170)
                                          (every? #(not (= (:web_name player) %)) ["Jesus"
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
                                                                      ]))
                                   false))
                               fpl-players)
        fpl-players-with-curr-team (map (fn [player]
                                          (if-let [player-in-curr-team (find-in-curr-team player)]
                                            (assoc player :current_team 1 :keep (:keep player-in-curr-team) :discard
                                                          (:discard  player-in-curr-team))
                                            (if-let [player-in-wanted-team (find-in-wanted-team player)]
                                              (assoc player :keep 1 :current_team 0)
                                              (assoc player :current_team 0 :keep 0))))
                                        fpl-players-per-mins)
        check-number-players-each-position (check-number-players-each-position fpl-players-with-curr-team)]
    (if (nil? check-number-players-each-position)
      (let [player-expected-values (merge-expected-values
                                     fpl-players-with-curr-team
                                     statsbomb-player-data
                                     statsbomb-team-data
                                     nil)
            players (map #(select-keys % [:web_name :now_cost :current_team :code :element_type :total_points :minutes
                                         :current_team :expected_value :team_code :keep :discard])
                        player-expected-values)]
        (json/write-str (merge (:body fpl-data) {:elements players :total_matches_played 23})))
      (println check-number-players-each-position))))

(defn handler [request]
  {:status 200
   :headers {"Content-Type" "application/json"}
   :body (calculate-expected-values)})

(defn -main [] (run-jetty handler {:port 3000}))