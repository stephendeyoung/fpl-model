(ns fpl.core-all-gws
  (:require [fpl.scoring :refer [rules]]
            [fpl.gw-data :as gw-data]
            [fpl.gw-xg :as gw-xg]
            [clojure.pprint :refer [pprint]]
            [fpl.appearance-pts :refer [player-appearance-pts]]
            [fpl.bonus-pts :refer [get-player-by-gw]]))

(def total-matches-played 29)
(def gw-trend-window 6)

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
                              ignore-appearances
                              & {:keys [expected-pts-symbol double-gw-team] :or {expected-pts-symbol :expected_points
                                                                                 double-gw-team      nil}}]
  (map (fn [player]

         (let [matching-sb-player (find-matching-sb-player player statsbomb-player-data)
               {team-xg-conceded :team_season_np_xg_conceded_pg
                team-id          :team_id} (first (filter #(= (:team_id matching-sb-player) (:team_id %))
                                                          statsbomb-team-data))
               {gsaa              :player_season_gsaa_90
                player-xg         :player_season_np_xg_90
                player-xga        :player_season_xa_90
                player-shots      :player_season_np_shots_90
                player-conversion :player_season_conversion_ratio
                double-gw?        :double_gw
                blank?            :blank_gw
                bonus-pts         :bonus-pts} matching-sb-player
               all-player-appearance-pts (first (filter #(= (:id %) (:id player)) player-appearance-pts)
                                                )
               appearance-pts-total (if all-player-appearance-pts
                                      (last (:average-pts all-player-appearance-pts))
                                      0)
               ;bonus-pts-total (if (= (:minutes player) 0)
               ;                  0
               ;                  (* (/ (:bonus player) (:minutes player)) 90))
               appearance-pts (cond
                                (true? ignore-appearances) 0
                                (true? double-gw?) (* appearance-pts-total 2)
                                (true? blank?) 0
                                :else appearance-pts-total)
               ;bonus-pts (cond
               ;            (true? double-gw?) (* bonus-pts-total 2)
               ;            (true? blank?) 0
               ;            :else bonus-pts-total)
               ]
           (if (not (nil? matching-sb-player))
             (let [expected-pts (cond
                                  (= (:element_type player) 1) {expected-pts-symbol ((:gk-scores rules) team-xg-conceded
                                                                                     gsaa
                                                                                     appearance-pts
                                                                                     bonus-pts)}
                                  (= (:element_type player) 2) {expected-pts-symbol ((:def-scores rules) player-xg
                                                                                     player-xga
                                                                                     team-xg-conceded
                                                                                     appearance-pts
                                                                                     bonus-pts)}
                                  (= (:element_type player) 3) {expected-pts-symbol ((:mid-scores rules) player-xg
                                                                                     player-xga
                                                                                     team-xg-conceded
                                                                                     appearance-pts
                                                                                     bonus-pts
                                                                                     player-shots
                                                                                     player-conversion)}
                                  (= (:element_type player) 4) {expected-pts-symbol ((:fwd-scores rules) player-xg
                                                                                     player-xga
                                                                                     appearance-pts
                                                                                     bonus-pts
                                                                                     player-shots
                                                                                     player-conversion)})]
               (-> player
                   (merge expected-pts)
                   (assoc :bonus-pts bonus-pts)))
             ;(clojure.pprint/pprint player)
             )))
       players))

(def current-team
  [{:name "Calvert-Lewin" :player_opta_id 177815 :keep 0 :discard 0}
   {:name "Jimenez" :player_opta_id 102057 :keep 0 :discard 0}
   {:name "Ings" :player_opta_id 84939 :keep 0}

   {:name "Sarr" :player_opta_id 232185 :keep 0}
   {:name "Martial" :player_opta_id 148225 :keep 0 :discard 0}
   {:name "De Bruyne" :player_opta_id 61366 :keep 0}
   {:name "McCarthy" :player_opta_id 50472 :keep 0 :discard 0}
   {:name "Mahrez" :player_opta_id 103025 :keep 0 :discard 0}

   {:name "Doherty" :player_opta_id 87835 :keep 0}
   {:name "Azpilicueta" :player_opta_id 41328 :keep 0 :discard 0}
   {:name "Saiss" :player_opta_id 107613 :keep 0 :discard 0}
   {:name "Alexander-Arnold" :player_opta_id 169187 :keep 0 :discard 0}
   {:name "Holgate" :player_opta_id 194164 :keep 0 :discard 0}

   {:name "Schmeichel" :player_opta_id 17745 :keep 0 :discard 0}
   {:name "McCarthy" :player_opta_id 58376 :keep 0}])

(def wanted-players
  [
   ;{:name "Aguero" :player_opta_id 37572 :keep 1}
   ;{:name "Salah" :player_opta_id 118748 :keep 0}
   ;{:name "Robertson" :player_opta_id 122798 :keep 1}
   ;{:name "Firmino" :player_opta_id 92217 :keep 1}
   ;{:name "Janmaat" :player_opta_id 52940 :keep 1}

   ;{:name "Guendouzi" :player_opta_id 242166 :keep 1}
   ;{:name "El Mohamady" :player_opta_id 37339 :keep 1}
   ;{:name "McCarthy" :player_opta_id 58376 :keep 1}
   ])

(defn- find-in-curr-team [player]
  (first (filter #(= (:player_opta_id %) (:code player)) current-team)))

(defn- find-in-wanted-team [player]
  (first (filter #(= (:player_opta_id %) (:code player)) wanted-players)))

(defn- calculate-trend [player prev-gameweeks]
  (let [pts-per-week (filter #(not (nil? %)) (map #(% player) prev-gameweeks))]
    (if (empty? pts-per-week)
      nil
      (- (last pts-per-week) (first (take-last gw-trend-window pts-per-week))))))

(defn- build-gw-str [gw]
  (let [gw-prefix (if (< gw 10)
                    "0"
                    "")]
    (str "gw" gw-prefix gw)))

(defn- build-expected-pts-str [data]
  (str (build-gw-str (:gw data)) "-expected-points"))

(defn fpl-players [fpl-data]
  ;(filter #(= (:web_name %) "Jiménez")
  (:elements fpl-data))

(defn- add-fpl-players-with-sb-id [fpl-data filtered-gw-data]
  (println "add-fpl-players-with-sb-id")
  (mapv (fn [player]
          (assoc player :player_id (:player_id (find-matching-sb-player player (:player-data (last filtered-gw-data))
                                                                        ))))
        (fpl-players fpl-data)))

(defn- merge-bonus-points [fpl-players sb-player-data gw]
  (let [player (first (filter (fn [fpl-player]
                                (= (:player_id fpl-player) (:player_id sb-player-data)))
                              fpl-players))
        bonus-pts (get-player-by-gw (:id player) (cond
                                                   (= gw 30) 39
                                                   (= gw 31) 40
                                                   (= gw 32) 41
                                                   :else gw))]
    (merge sb-player-data {:bonus-pts (if bonus-pts
                                        (:bonus-average bonus-pts)
                                        0)})))

(defn- add-player-expected-points-per-gw [filtered-gw-data fpl-players-with-sb-id ignore-appearances]
  (println "add-player-expected-points-per-gw")
  (mapv (fn [data]
          (filter #(not (nil? %))
                  (merge-expected-values fpl-players-with-sb-id
                                         (map #(merge-bonus-points fpl-players-with-sb-id % (:gw data))
                                              (:player-data data))
                                         (mapv (fn [team]
                                                 (assoc team
                                                   :team_season_np_xg_conceded_pg
                                                   [(:team_season_np_xg_conceded_pg team)]))
                                               (:team-data data))
                                         ignore-appearances
                                         :expected-pts-symbol (keyword (build-gw-str (:gw data))))))
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

(defn- add-players-trend [player-expected-points-joined prev-gameweeks]
  (println "add-players-ema")
  (mapv (fn [player]
          (assoc player :trend (calculate-trend player prev-gameweeks)))
        player-expected-points-joined))

(defn- add-player-expected-points-future-gws [player-expected-points-joined filtered-gw-data fixture-data
                                              latest-fixture fixtures ignore-appearances test?]
  (println "add-player-expected-points-future-gws")
  (mapv (fn [data]
          (filter #(not (nil? %))
                  (merge-expected-values player-expected-points-joined
                                         (:player-data data)
                                         (:team-data data)
                                         ignore-appearances
                                         :expected-pts-symbol (keyword (build-expected-pts-str data)))))
        (gw-xg/expected-gw-points player-expected-points-joined fixture-data filtered-gw-data latest-fixture fixtures
                                  test?)))

(defn- join-player-expected-points-future-gws [player-expected-points-future-gws]
  (println "join-player-expected-points-future-gws")
  (reduce (fn [gw-list1 gw-list2]
            (mapv
              (fn [player]
                (merge player (first (filter #(= (:code %) (:code player)) gw-list1))))
              gw-list2))
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

(defn calculate-expected-values [fpl-data fixture-data to-gw latest-fixture fixtures
                                 & {:keys [ignore-appearances test?]}]
  (let [prev-gameweeks (map #(keyword (build-gw-str %))
                            (range 9 (+ latest-fixture 1)))
        filtered-gw-data (filter #(<= (:gw %) to-gw) gw-data/gameweek-data)
        fpl-players-with-sb-id (add-fpl-players-with-sb-id fpl-data filtered-gw-data)
        player-expected-points-per-gw (add-player-expected-points-per-gw filtered-gw-data fpl-players-with-sb-id ignore-appearances)
        player-expected-points-joined (join-expected-points player-expected-points-per-gw)
        players-with-trend (add-players-trend player-expected-points-joined prev-gameweeks)
        player-expected-points-future-gws (add-player-expected-points-future-gws players-with-trend filtered-gw-data
                                                                                 fixture-data latest-fixture
                                                                                 fixtures ignore-appearances test?)
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
        check-number-players-each-position (check-number-players-each-position fpl-players-with-curr-team)
        expected-points-keys (map (fn [fixture]
                                    (keyword (str "gw" fixture "-expected-points")))
                                  fixtures)]
    (if (nil? check-number-players-each-position)
      (let [players (map (fn [player]
                           (let [plyr (select-keys player (concat
                                                            [:web_name
                                                             :now_cost
                                                             :current_team
                                                             :code
                                                             :element_type
                                                             :total_points
                                                             :minutes
                                                             :current_team
                                                             :expected_points
                                                             :team_code
                                                             :keep
                                                             :discard
                                                             :trend
                                                             :expected-points-total
                                                             :bonus
                                                             :bonus-pts]
                                                            prev-gameweeks
                                                            expected-points-keys))]
                             (assoc plyr :expected_points_per_90
                                         ((keyword (str "gw" latest-fixture)) plyr))))
                         fpl-players-with-curr-team)]
        (merge (:body fpl-data) {:elements players :total_matches_played total-matches-played}))
      (println check-number-players-each-position))))