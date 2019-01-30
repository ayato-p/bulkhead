(ns org.panchromatic.bulkhead
  (:require [integrant.core :as ig]
            [org.panchromatic.bulkhead.internal :as internal]
            [org.panchromatic.bulkhead.state :as state]))

(defn set-prep! [f]
  (alter-var-root #'state/prep (constantly f)))

(defmacro with-bulkhead
  {:arglists '([bindings mock-map body])}
  [& args]
  (let [[bindings mock-map body] (internal/parse-args args &env)
        prep (internal/detect-prep-fn &env)]
    `(let [mock-map# ~mock-map
           org&mock-keypairs# (internal/make-keypairs (keys mock-map#))
           start-keys# (-> (reduce-kv #(-> (disj %1 %2) (conj %3))
                                      ~(-> bindings vals set)
                                      org&mock-keypairs#))]
       (internal/create-mock-components! org&mock-keypairs# mock-map#)
       (try
         (let [~'$system (-> (reduce-kv #(-> (dissoc %1 %2) (assoc %3 {}))
                                        (~prep)
                                        org&mock-keypairs#)
                             (ig/init start-keys#))
               ~@(internal/bind-components '$system bindings)]
           (try
             ~@body
             (finally
               (ig/halt! ~'$system start-keys#))))
         (finally
           (internal/destroy-mock-components! org&mock-keypairs#))))))
