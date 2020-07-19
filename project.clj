(defproject fpl-model "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [clj-http "3.10.1"]
                 [cheshire "5.9.0"]
                 [org.clojure/data.json "0.2.6"]
                 [ring/ring-core "1.6.3"]
                 [ring/ring-jetty-adapter "1.6.3"]
                 [org.clojure/data.csv "1.0.0"]]
  :repl-options {}
  :plugins [[lein-cljfmt "0.6.4"]]
  :resource-paths ["../resources"])
