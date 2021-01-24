(ns fpl.core-all-gws
  (:require [fpl.scoring :refer [rules]]
            [fpl.gw-data :as gw-data]
            [fpl.gw-xg :as gw-xg]
            [clojure.pprint :refer [pprint]]
            [fpl.appearance-pts :refer [player-appearance-pts]]
            [fpl.bonus-pts :refer [get-player-by-gw]]
            [clojure.string :as string]))

(def total-matches-played 29)
(def gw-trend-window 6)

(def players-missing-opta-id
  [{:name    "James Justin"
    :sb-id   5821537
    :opta-id 220627}
   {:name    "Yerry Mina"
    :sb-id   5636302
    :opta-id 164511}
   {:name    "Miguel Almiron"
    :sb-id   6602488
    :opta-id 179018}
   {:name    "Samatta"
    :sb-id   6939315
    :opta-id 217487}
   {:name    "Anthony Gordon"
    :sb-id   5317174
    :opta-id 232826}
   {:name    "Steven Alzate"
    :sb-id   5823507
    :opta-id 235382}
   {:name    "Thomas Soucek"
    :sb-id   5317442
    :opta-id 215439}
   {:name    "Andre Anguissa"
    :sb-id   293265
    :opta-id 203325}
   {:name    "Robert Sanchez"
    :sb-id   5823522
    :opta-id 215059}
   {:name    "Ivan Cavaleiro"
    :sb-id   7065915
    :opta-id 166324}
   {:name    "Fabio Silva"
    :sb-id   12464861
    :opta-id 449988}])

(def penalty-probability 0.16)
(def penalty-score-probability 0.8)

(def penalty-takers
  [{:team        "Arsenal"
    :name        "Aubameyang"
    :sb-id       21536
    :opta-id     54694
    :probability 1}
   #_{:team        "Aston Villa"
      :name        "Watkins"
      :sb-id       3467805
      :opta-id     178301
      :probability 1}
   ;{:team        "Brighton"
   ; :name        "Groß"
   ; :sb-id       5866
   ; :opta-id     60307
   ; :probability 1}
   {:team        "Burnley"
    :name        "Wood"
    :sb-id       8956
    :opta-id     60689
    :probability 0.5}
   {:team        "Burnley"
    :name        "Barnes"
    :sb-id       5935
    :opta-id     44699
    :probability 0.5}
   {:team        "Chelsea"
    :name        "Werner"
    :sb-id       24320
    :opta-id     165153
    :probability 1}
   {:team        "Crystal Palace"
    :name        "Zaha"
    :sb-id       17190
    :opta-id     82403
    :probability 1}
   {:team        "Everton"
    :name        "Richarlison"
    :sb-id       972691
    :opta-id     212319
    :probability 1}
   {:team        "Fulham"
    :name        "Mitrovic"
    :sb-id       14217
    :opta-id     128389
    :probability 1}
   {:team        "Leeds"
    :name        "Bamford"
    :sb-id       6949
    :opta-id     106617
    :probability 0.5}
   {:team        "Leicester"
    :name        "Vardy"
    :sb-id       8893
    :opta-id     101668
    :probability 1}
   {:team        "Liverpool"
    :name        "Salah"
    :sb-id       44086
    :opta-id     118748
    :probability 1}
   ;{:team        "Manchester City"
   ; :name        "De Bruyne"
   ; :sb-id       14140
   ; :opta-id     61366
   ; :probability 1}
   {:team        "Manchester City"
    :name        "Gundogan"
    :sb-id       21497
    :opta-id     59859
    :probability 1}
   {:team        "Manchester United"
    :name        "Fernandes"
    :sb-id       23174
    :opta-id     141746
    :probability 1}
   {:team        "Newcastle United"
    :name        "Wilson"
    :sb-id       288352
    :opta-id     75115
    :probability 1}
   {:team        "Sheffield United"
    :name        "Berge"
    :sb-id       622611
    :opta-id     207189
    :probability 0.75}
   {:team        "Sheffield United"
    :name        "Lundstram"
    :sb-id       12285
    :opta-id     153723
    :probability 0.25}
   {:team        "Southampton"
    :name        "Ings"
    :sb-id       5925
    :opta-id     84939
    :probability 1}
   ;{:team        "Southampton"
   ; :name        "Ward-Prowse"
   ; :sb-id       18330
   ; :opta-id     101178
   ; :probability 1}
   {:team        "Spurs"
    :name        "Kane"
    :sb-id       19838
    :opta-id     78830
    :probability 1}
   {:team        "West Brom"
    :name        "Pereira"
    :sb-id       292856
    :opta-id     210407
    :probability 1}
   {:team        "West Ham"
    :name        "Antonio"
    :sb-id       10356
    :opta-id     57531
    :probability 1}
   {:team        "Wolves"
    :name        "Jiménez"
    :sb-id       12830
    :opta-id     102057
    :probability 1}])

(defn- find-matching-sb-player [player statsbomb-data]
  ;(when (= (:web_name player) "Rodriguez")
  ;  (let [soucek (first (filter #(= (:player_id %) 17157) statsbomb-data))]
  ;    (println "hey")))
  (let [player-missing-opta-id (first (filter #(= (:code player) (:opta-id %)) players-missing-opta-id))]
    (first (filter (fn [sb-player]
                     (or (= (:player_opta_id sb-player) (:code player))
                         (= (:player_id player) (:player_id sb-player))
                         (= (:player_id sb-player) (:sb-id player-missing-opta-id))))
                   ;(= (:second_name player) (:player_last_name %))
                   ;(if (not (nil? (:name %)))
                   ;  (= (:second_name player) (last (clojure.string/split (:name %) #" ")))
                   ;  false))
                   statsbomb-data))))

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

         (let [matching-sb-player (first (filter #(= (:player_id player) (:player_id %))
                                                 statsbomb-player-data))
               {team-xg-conceded :team_season_np_xg_conceded_pg
                team-xg          :team_season_np_xg_pg
                team-id          :team_id} (first (filter #(= (:team_id matching-sb-player) (:team_id %))
                                                          statsbomb-team-data))
               {gsaa              :player_season_gsaa_90
                player-xg         :player_season_np_xg_90
                player-xa         :player_season_xa_90
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
               expected-pts-symbol-name (name expected-pts-symbol)
               penalty-taker (first (filter #(= (:player_id player) (:sb-id %)) penalty-takers))
               penalty-probability (when penalty-taker
                                     (* (:probability penalty-taker) (* penalty-probability
                                                                        penalty-score-probability)))
               penalty-player-score-probability (cond
                                                  blank? 0
                                                  (and double-gw? penalty-taker) (* penalty-probability 2)
                                                  penalty-taker penalty-probability
                                                  :else 0)
               penalty-miss-probability (when penalty-taker
                                          (* penalty-probability (- 1 penalty-score-probability)))
               penalty-player-miss-probability (cond
                                                 blank? 0
                                                 (and double-gw? penalty-taker) (* penalty-miss-probability 2)
                                                 penalty-taker penalty-miss-probability
                                                 :else 0)
               ]
           (if (not (nil? matching-sb-player))
             (let [expected-pts (cond
                                  (= (:element_type player) 1) {expected-pts-symbol ((:gk-scores rules) team-xg-conceded
                                                                                     gsaa
                                                                                     appearance-pts
                                                                                     0)}
                                  (= (:element_type player) 2) {expected-pts-symbol ((:def-scores rules) player-xg
                                                                                     player-xa
                                                                                     team-xg-conceded
                                                                                     appearance-pts
                                                                                     0
                                                                                     penalty-player-score-probability
                                                                                     penalty-player-miss-probability)}
                                  (= (:element_type player) 3) {expected-pts-symbol ((:mid-scores rules) player-xg
                                                                                     player-xa
                                                                                     team-xg-conceded
                                                                                     appearance-pts
                                                                                     0
                                                                                     penalty-player-score-probability
                                                                                     penalty-player-miss-probability)}
                                  (= (:element_type player) 4) {expected-pts-symbol ((:fwd-scores rules) player-xg
                                                                                     player-xa
                                                                                     appearance-pts
                                                                                     0
                                                                                     penalty-player-score-probability
                                                                                     penalty-player-miss-probability)})]
               (-> player
                   (merge expected-pts {(keyword (str expected-pts-symbol-name "-xg"))       player-xg
                                        (keyword (str expected-pts-symbol-name "-xa"))       player-xa
                                        (keyword (str expected-pts-symbol-name "-gsaa"))     gsaa
                                        (keyword (str expected-pts-symbol-name "-team-xg"))  team-xg
                                        (keyword (str expected-pts-symbol-name "-team-xga")) team-xg-conceded
                                        (keyword (str expected-pts-symbol-name "-minutes")) (:player_season_minutes matching-sb-player)
                                        })
                   ;(update :player_season_minutes (fn [mins]
                   ;                                 (println "(:player_season_minutes matching-sb-player)" (:player_season_minutes matching-sb-player))
                   ;                                 (if (nil? mins)
                   ;                                   (:player_season_minutes matching-sb-player)
                   ;                                   nil)))
                   (assoc :bonus-pts 0)))
             ;(when (= (:web_name player) "Rodriguez")
             ;  (println "hey"))
             )))
       players))

(def current-team
  [
   ; FPL team
   ;{:name "DCL" :player_opta_id 177815 :keep 0 :price 78}
   ;{:name "Bamford" :player_opta_id 106617 :keep 0 :discard 0 :price 63}
   ;{:name "Adams" :player_opta_id 200439 :keep 0 :discard 0 :price 60}
   ;
   ;{:name "Gross" :player_opta_id 60307 :keep 0 :discard 0 :price 58}
   ;{:name "Son" :player_opta_id 85971 :keep 0 :discard 0 :price 96}
   ;{:name "Salah" :player_opta_id 118748 :keep 1 :price 123}
   ;{:name "Grealish" :player_opta_id 114283 :keep 0 :discard 0 :price 77}
   ;{:name "De Bruyne" :player_opta_id 61366 :keep 0 :discard 0 :price 117}
   ;
   ;{:name "Justin" :player_opta_id 220627 :keep 0 :discard 0 :price 47}
   ;{:name "Cancelo" :player_opta_id 121145 :keep 0 :discard 0 :price 55}
   ;{:name "Taylor" :player_opta_id 103914 :keep 0 :price 45}
   ;{:name "Ayling" :player_opta_id 66588 :keep 0 :discard 0 :price 45}
   ;{:name "Mitchell" :player_opta_id 244723 :keep 0 :discard 0 :price 39}
   ;
   ;{:name "Nyland" :player_opta_id 98770 :keep 1 :discard 0 :price 45}
   ;{:name "McCarthy" :player_opta_id 58376 :keep 1 :price 46}

   ; Fanteam
   ;{:name "DCL" :player_opta_id 177815 :keep 0 :price 85}
   ;{:name "Adams" :player_opta_id 200439 :keep 0 :discard 0 :price 61}
   ;{:name "Connolly" :player_opta_id 233425 :keep 0 :discard 1 :price 47}
   ;
   ;{:name "Gross" :player_opta_id 60307 :keep 0 :discard 0 :price 54}
   ;{:name "Sterling" :player_opta_id 103955 :keep 0 :discard 0 :price 109}
   ;{:name "Salah" :player_opta_id 118748 :keep 0 :price 125}
   ;{:name "Zaha" :player_opta_id 82403 :keep 0 :discard 0 :price 70}
   ;{:name "Son" :player_opta_id 85971 :keep 0 :discard 0 :price 105}
   ;
   ;{:name "Dias" :player_opta_id 171314 :keep 0 :discard 0 :price 50}
   ;{:name "Cancelo" :player_opta_id 121145 :keep 0 :discard 0 :price 50}
   ;{:name "Taylor" :player_opta_id 103914 :keep 1 :price 45}
   ;{:name "Chilwell" :player_opta_id 172850 :keep 0 :discard 0 :price 56}
   ;{:name "Mitchell" :player_opta_id 244723 :keep 1 :discard 0 :price 41}
   ;
   ;{:name "Nyland" :player_opta_id 98770 :keep 1 :discard 0 :price 40}
   ;{:name "McCarthy" :player_opta_id 58376 :keep 1 :price 46}

   ;Algorithm team
   ;{:name "Bamford" :player_opta_id 106617 :keep 0 :discard 0 :price 63}
   ;{:name "Adams" :player_opta_id 200439 :keep 0 :discard 0 :price 60}
   ;{:name "Brewster" :player_opta_id 195473 :keep 0 :discard 1 :price 45}
   ;
   ;{:name "Zaha" :player_opta_id 82403 :keep 0 :discard 0 :price 75}
   ;{:name "Mahrez" :player_opta_id 103025 :keep 0 :discard 0 :price 83}
   ;{:name "Salah" :player_opta_id 118748 :keep 0 :price 122}
   ;{:name "Gross" :player_opta_id 60307 :keep 0 :discard 0 :price 58}
   ;{:name "Mane" :player_opta_id 110979 :keep 0 :discard 0 :price 119}
   ;
   ;{:name "TAA" :player_opta_id 169187 :keep 0 :discard 0 :price 72}
   ;{:name "Cancelo" :player_opta_id 121145 :keep 0 :discard 0 :price 54}
   ;{:name "Walker" :player_opta_id 58621 :keep 0 :discard 0 :price 61}
   ;{:name "Vinagre" :player_opta_id 216054 :keep 1 :discard 0 :price 41}
   ;{:name "Mitchell" :player_opta_id 244723 :keep 1 :discard 0 :price 39}
   ;
   ;{:name "Nyland" :player_opta_id 98770 :keep 1 :discard 0 :price 40}
   ;{:name "McCarthy" :player_opta_id 58376 :keep 1 :price 45}

   ; best model team
   ;{:name "Brewster" :player_opta_id 195473 :keep 1 :price 45}
   ;
   ;{:name "Kilman" :player_opta_id 214048 :keep 1 :price 43}
   ;{:name "Mitchell" :player_opta_id 244723 :keep 1 :price 41}
   ;
   ;{:name "Nyland" :player_opta_id 98770 :keep 1 :price 40}
   ;{:name "Ryan" :player_opta_id 131897 :keep 1 :price 45}
   ])

(def wanted-players
  [
   ;{:name "Mane" :player_opta_id 110979 :keep 1}
   ;{:name "Fernandes" :player_opta_id 141746 :keep 1}
   ;{:name "Robertson" :player_opta_id 122798 :keep 1}
   ;{:name "James" :player_opta_id 225796 :keep 1}
   ;{:name "Chilwell" :player_opta_id 172850 :keep 0}
   ;{:name "Rodrigo" :player_opta_id 80954 :keep 1}
   ;{:name "Sterling" :player_opta_id 103955 :keep 1}
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

(defn- add-fpl-players-with-sb-id [fpl-data filtered-gw-data]
  (println "add-fpl-players-with-sb-id")
  (mapv (fn [player]
          (let [matching-player (find-matching-sb-player player (:player-data (last filtered-gw-data)))]
            (when (nil? matching-player)
              (println (:web_name player)))
            (assoc player :player_id (:player_id matching-player))))
        fpl-data))

(defn- merge-bonus-points [fpl-players sb-player-data gw]
  (let [player (first (filter (fn [fpl-player]
                                (= (:player_id fpl-player) (:player_id sb-player-data)))
                              fpl-players))
        bonus-pts (get-player-by-gw (:id player) (cond
                                                   (= gw 30) 39
                                                   (= gw 31) 40
                                                   (= gw 32) 41
                                                   (= gw 33) 42
                                                   (= gw 34) 43
                                                   (= gw 35) 44
                                                   (= gw 36) 45
                                                   (= gw 37) 46
                                                   (= gw 38) 47
                                                   :else gw))]
    (merge sb-player-data {:bonus-pts (if bonus-pts
                                        (:bonus-average bonus-pts)
                                        0)})))

(defn- add-player-expected-points-per-gw [filtered-gw-data fpl-players-with-sb-id ignore-appearances]
  (println "add-player-expected-points-per-gw")
  (mapv (fn [data]
          (filter (fn [player]
                    ;(when (= (:code player) 60025)
                    ;  (println "hey"))
                    (not (nil? player)))
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
                ;(when (= (:code player) 60025)
                ;  (println "hey2"))
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
          (filter (fn [player]
                    ;(when (= (:code player) 60025)
                    ;  (println "hey3"))
                    (not (nil? player)))
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
                ;(when (= (:code player) 60025)
                ;  (println "hey4"))
                (merge player (first (filter #(= (:code %) (:code player)) gw-list1))))
              gw-list2))
          player-expected-points-future-gws))

(defn- add-player-expected-points-future-gws-total [player-expected-points-future-gws-joined fixtures]
  (println "add-player-expected-points-future-gws-total")
  (mapv (fn [player]
          ;(when (= (:code player) 60025)
          ;  (println "hey5"))
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
                            (range 1 (+ latest-fixture 1)))
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
        ;fpl-players-with-curr-team (map (fn [player]
        ;                                  (if-let [player-in-curr-team (find-in-curr-team player)]
        ;                                    (assoc player :current_team 1 :keep (:keep player-in-curr-team)
        ;                                                  :discard (:discard player-in-curr-team)
        ;                                                  :now_cost (:price player-in-curr-team))
        ;                                    (if-let [player-in-wanted-team (find-in-wanted-team player)]
        ;                                      (assoc player :keep 1 :current_team 0)
        ;                                      (assoc player :current_team 0 :keep 0))))
        ;                                player-expected-points-future-gws-total)
        check-number-players-each-position (check-number-players-each-position player-expected-points-future-gws-total)
        ]
    (if (nil? check-number-players-each-position)
      (let [players (map (fn [player]
                           (let [gw-keys (filter (fn [player-key]
                                                   (string/includes? (name player-key)
                                                                     "gw"))
                                                 (keys player))
                                 plyr (select-keys player (concat
                                                            [:web_name
                                                             :first_name
                                                             :now_cost
                                                             :current_team
                                                             :code
                                                             :element_type
                                                             :total_points
                                                             :minutes
                                                             :player_season_minutes
                                                             :current_team
                                                             :expected_points
                                                             :team_code
                                                             :keep
                                                             :discard
                                                             :trend
                                                             :expected-points-total
                                                             :bonus
                                                             :bonus-pts]
                                                            ;prev-gameweeks
                                                            gw-keys))]
                             (-> plyr
                                 (assoc :expected_points_per_90 ((keyword (build-gw-str latest-fixture)) plyr))
                                 (assoc :player_season_minutes ((keyword (str (build-gw-str latest-fixture)
                                                                              "-minutes")) plyr)))))
                         player-expected-points-future-gws-total)]
        (merge (:body fpl-data) {:elements players :total_matches_played total-matches-played}))
      (println check-number-players-each-position))))