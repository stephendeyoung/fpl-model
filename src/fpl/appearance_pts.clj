(ns fpl.appearance-pts
  (:require [fpl.fpl-data-csv :refer [fpl-csv-data]]))

(def ema-weighting 0.2)

(defn- ema3 [c a]
  (loop [ct (rest c) res [(first c)]]
    (if (= (count ct) 0)
      res
      (recur
        (rest ct)
        (into
          res
          [(+ (* a (first ct)) (* (- 1 a) (peek res)))])))))


(def player-appearance-pts
  (->> fpl-csv-data
       (map (fn [record]
              (select-keys record [:name :element :GW :minutes])))
       ;(group-by (juxt :element :GW))
       ;(map (fn [[_ data]]
       ;       (when (> (count data) 1)
       ;         (println data))
       ;       (first data)))
       (group-by :element)
       (map (fn [[player-id player-data]]
              (let [pts-each-game (->> player-data
                                       (map :minutes)
                                       (map (fn [minutes]
                                              (cond
                                                (>= minutes 60) 2
                                                (> minutes 0) 1
                                                :else 0))))]
                [player-id pts-each-game])))
       (map (fn [[player-id pts-each-game]]
              {:id player-id
               :pts-per-game pts-each-game
               :average-pts (ema3 pts-each-game ema-weighting)}))
       ))

;(clojure.pprint/pprint (first (filter #(= (:id %) 14) player-appearance-pts)))
;(clojure.pprint/pprint (first (filter #(= (:id %) 211) player-appearance-pts)))
