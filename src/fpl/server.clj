(ns fpl.server
  (:require [fpl.core-all-gws :refer [calculate-expected-values]]
            [ring.adapter.jetty :refer [run-jetty]]
            [clojure.data.json :as json]
            [clj-http.client :as client]))

(defn- get-fpl-data []
  (client/get "https://fantasy.premierleague.com/api/bootstrap-static/" {:as :json}))

(def fpl-data
  (:body (get-fpl-data)))

(defn handler [request]
  {:status  200
   :headers {"Content-Type" "application/json"}
   :body    (json/write-str (calculate-expected-values)))

(defn -main [] (run-jetty handler {:port 3000}))
