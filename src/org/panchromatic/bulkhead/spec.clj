(ns org.panchromatic.bulkhead.spec
  (:require [clojure.spec.alpha :as s]))

(s/def ::composite-key
  (s/and vector?
         (partial every? qualified-keyword?)))

(s/def ::config-key
  (s/and (s/or :qualified-keyword qualified-keyword?
               :composite-key ::composite-key)
         (s/conformer second)))

(s/def ::bindings
  (s/* (s/cat :sym simple-symbol?
              :key ::config-key)))

(s/def ::function
  ;; TODO more strict
  (s/and (s/or :fn-literal sequential?
               :symbol symbol?)
         (s/conformer second)))

(s/def ::mock-map
  (s/map-of ::config-key
            ::function))

(s/def ::with-bulkhead-args
  (s/cat :bindings (s/spec ::bindings)
         :mock-map ::mock-map
         :body (s/? (s/* any?))))

(defn throw-spec-error [spec x]
  (throw (ex-info (s/explain-str spec x)
                  (s/explain-data spec x)
                  )))
