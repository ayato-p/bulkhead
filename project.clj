(defproject org.panchromatic/bulkhead "0.1.0-SNAPSHOT"
  :description "Testing library for Clojure and Integrant application"
  :url "https://github.com/ayato-p/bulkhead"
  :license {:name "MIT License"
            :url "https://choosealicense.com/licenses/mit"}
  :dependencies [[integrant "0.7.0"]
                 [org.clojure/tools.namespace "0.2.11"]]
  :profiles
  {:provided
   {:dependencies [[org.clojure/clojure "1.9.0"]]}
   :R1.9
   {:dependencies [[org.clojure/clojure "1.9.0"]]}
   :R1.10
   {:dependencies [[org.clojure/clojure "1.10.0-beta5"]]}}

  :aliases
  {"all-test" ["with-profiles" "R1.9:R1.10" "test"]})
