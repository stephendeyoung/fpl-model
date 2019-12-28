; weighting towards more value rather than selecting 15 best!
; data from previous seasons
; disciplinary record
;

(ns fpl.core
  (:require [clj-http.client :as client]
            [fpl.scoring :refer [rules]]
            [clojure.edn :as edn]
            [clojure.data.json :as json]
            [ring.adapter.jetty :refer [run-jetty]]))

(defn- get-fpl-data []
  (client/get "https://fantasy.premierleague.com/api/bootstrap-static/" {:as :json}))

(defn- read-statsbomb-data [filename]
  (edn/read-string (slurp (str "resources/" filename))))

(defn- find-matching-sb-player [player statsbomb-data]
  (first (filter #(= (:player_opta_id %) (:code player)) statsbomb-data)))

(defn- check-number-players-each-position [players]
  (let [{goalkeepers :1 defenders :2 midfielders :3 forwards :4}  (into {} (for [[k v] (group-by :element_type players)]
                                                                          [(keyword (str k)) (count v)])
                                                      )]
    (cond
      (< goalkeepers 2) "Not enough goalkeepers"
      (< defenders 5) "Not enough defenders"
      (< midfielders 5) "Not enough midfielders"
      (< forwards 3) "Not enough forwards"
      :else nil)))

(defn- merge-expected-values [players statsbomb-player-data statsbomb-team-data]
  (map (fn [player]

         (let [matching-sb-player (find-matching-sb-player player statsbomb-player-data)
               {team-xg-conceded :team_season_np_xg_conceded_pg} (first (filter #(= (:team_id matching-sb-player) (:team_id %))
                                                                                statsbomb-team-data))
               {gsaa :player_season_gsaa_90
                player-xg :player_season_np_xg_90
                player-xga :player_season_xa_90
                player-shots :player_season_np_shots_90
                player-conversion :player_season_conversion_ratio} matching-sb-player]

           (cond
             (= (:element_type player) 1) (merge player {:expected_value ((:gk-scores rules) team-xg-conceded gsaa)})
             (= (:element_type player) 2) (merge player {:expected_value ((:def-scores rules) player-xg player-xga
                                                                          team-xg-conceded)
                                                         })
             (= (:element_type player) 3) (merge player {:expected_value ((:mid-scores rules) player-xg player-xga
                                                                          team-xg-conceded player-shots player-conversion)
                                                         })
             (= (:element_type player) 4) (merge player {:expected_value ((:fwd-scores rules) player-xg player-xga player-shots player-conversion)
                                                         }))))
       players))

(defn- calculate-expected-values []
  (let [fpl-data (get-fpl-data)
        fpl-players (-> fpl-data
                        :body
                        :elements)
        statsbomb-player-data (read-statsbomb-data "statsbomb-player-data-gw20.edn")
        statsbomb-team-data (read-statsbomb-data "statsbomb-team-data-gw20.edn")
        fpl-players-per-mins (filter
                               (fn [player]
                                 (if-let [matching-sb-player (find-matching-sb-player player statsbomb-player-data)]
                                   (or
                                     (and (>= (:minutes matching-sb-player) 1170)
                                          (every? #(not (= (:web_name player) %)) ["De Bruyne"]))
                                     (some #(= (:web_name player) %) ["Jesus" "Pulisic" "Salah"]))
                                   false))
                               fpl-players)
        check-number-players-each-position (check-number-players-each-position fpl-players-per-mins)]
    (if (nil? check-number-players-each-position)
      (let [player-expected-values (merge-expected-values
                                                                                fpl-players-per-mins
                                                                                         statsbomb-player-data
                                                                                         statsbomb-team-data)]
        (doseq [player (sort-by :expected_value >
                                (map #(select-keys % [:web_name :expected_value])
                                     player-expected-values))]
          (println player))
        (json/write-str (merge (:body fpl-data) {:elements player-expected-values :total_matches_played 19})))
      (println check-number-players-each-position))))

(defn handler [request]
  {:status 200
   :headers {"Content-Type" "application/json"}
   :body (calculate-expected-values)})

(defn -main [] (run-jetty handler {:port 3000}))
