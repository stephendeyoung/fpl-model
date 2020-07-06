(ns fpl.fpl-data-csv
  (:require [clojure.java.io :as io]
            [clojure.data.csv :as csv]))

(defn- read-csv []
  (with-open [reader (io/reader "../resources/fpl_data/all-gws-to-41.csv")]
    (doall
      (csv/read-csv reader))))

(defn- csv-data->maps [csv-data]
  (map zipmap
       (->> (first csv-data)                                ;; First row is the header
            (map keyword)                                   ;; Drop if you want string keys instead
            repeat)
       (rest csv-data)))

(def fpl-csv-data
  (->> (read-csv)
       csv-data->maps
       (map (fn [csv-record]
              (-> csv-record
                  (update :minutes #(Long/parseLong %))
                  (update :element #(Long/parseLong %))
                  (update :bonus #(Long/parseLong %))
                  (update :was_home (fn [val]
                                      (if (= val "True")
                                        true
                                        false)))
                  (update :GW #(Integer/parseInt %)))))))
