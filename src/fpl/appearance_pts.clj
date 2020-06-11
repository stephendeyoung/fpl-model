(ns fpl.appearance-pts
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]))

(def ema-weighting 0.2)

(defn- read-csv []
  (with-open [reader (io/reader "../resources/fpl_data/all-gws-to-29.csv")]
    (doall
      (csv/read-csv reader))))

(defn- csv-data->maps [csv-data]
  (map zipmap
       (->> (first csv-data)                                ;; First row is the header
            (map keyword)                                   ;; Drop if you want string keys instead
            repeat)
       (rest csv-data)))

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
  (->> (read-csv)
       csv-data->maps
       (map (fn [csv-record]
              (-> csv-record
                  (update :minutes #(Long/parseLong %))
                  (update :element #(Long/parseLong %)))))
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
