(ns fpl.bonus-pts
  (:require [fpl.fpl-data-csv :refer [fpl-csv-data]]))

(def player-bonus-pts
  (->> fpl-csv-data
       (map (fn [record]
              (select-keys record [:name :element :GW :bonus :was_home :minutes])))
       (group-by (juxt :element :was_home))
       (map (fn [[[player-id home?] player-data]]
              (let [total-mins (reduce #(+ (:minutes %2) %1) 0 player-data)
                    total-bonus (reduce #(+ (:bonus %2) %1) 0 player-data)]
                {:id            player-id
                 :name          (:name (first player-data))
                 :home?         home?
                 :bonus-average (if (= total-mins 0)
                                  0
                                  (* (double (/ total-bonus total-mins)) 90))})))
       ))

(def player-bonus-pts-by-gw
  (->> fpl-csv-data
       (map (fn [record]
              (select-keys record [:name :element :GW :bonus :was_home :minutes])))
       (group-by :element)))

(defn get-player-bonus [id home?]
  (first (filter (fn [player-data]
                   (and (= (:id player-data) id)
                        (= (:home? player-data) home?)))
                 player-bonus-pts)))

(defn get-player-by-gw [id max-gw]
  (->> player-bonus-pts-by-gw
       (filter (fn [[player-id _]]
                 (= player-id id)))
       (map (fn [[player-id player-data]]
              (let [player-data-gws (filter #(<= (:GW %) max-gw)
                                            player-data)
                    total-mins (reduce #(+ (:minutes %2) %1) 0 player-data-gws)
                    total-bonus (reduce #(+ (:bonus %2) %1) 0 player-data-gws)]
                {:id            player-id
                 :bonus-average (if (= total-mins 0)
                                  0
                                  (* (double (/ total-bonus total-mins)) 90))})))
       (first)))

; Tammy Abraham
;(println (filter #(= (:id %) 460) player-bonus-pts))