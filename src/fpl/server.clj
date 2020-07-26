(ns fpl.server
  (:require [fpl.core-all-gws :refer [calculate-expected-values]]
            [ring.adapter.jetty :refer [run-jetty]]
            [clojure.data.json :as json]
            [clj-http.client :as client]
            [clojure.edn :as edn]))

(defn- get-fpl-data []
  (client/get "https://fantasy.premierleague.com/api/bootstrap-static/" {:as :json}))

(def fixtures
  (edn/read-string (slurp "../resources/fixtures.edn")))

(def fpl-data
  (:body (get-fpl-data)))

(def results
  (calculate-expected-values fpl-data fixtures 38 37 [38] :ignore-appearances true :test? false))

(defn handler [request]
  {:status  200
   :headers {"Content-Type" "application/json"}
   :body    (json/write-str results)})

(defn -main [] (run-jetty handler {:port 3000}))
