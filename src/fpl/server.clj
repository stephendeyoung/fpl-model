(ns fpl.server
  (:require [fpl.core-all-gws :refer [calculate-expected-values]]
            [ring.adapter.jetty :refer [run-jetty]]
            [clojure.data.json :as json]
            [clj-http.client :as client]))

(defn- get-fpl-data []
  (client/get "https://fantasy.premierleague.com/api/bootstrap-static/" {:as :json}))

(def fpl-data
  (:body (get-fpl-data)))

(def results
  (calculate-expected-values fpl-data 30 29 [30] false))

(defn handler [request]
  {:status  200
   :headers {"Content-Type" "application/json"}
   :body    (json/write-str results)})

(defn -main [] (run-jetty handler {:port 3000}))
