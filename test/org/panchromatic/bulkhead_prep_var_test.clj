(ns org.panchromatic.bulkhead-prep-var-test
  (:require [org.panchromatic.bulkhead :as sut]
            [clojure.test :as t]
            [integrant.core :as ig]))

(defmethod ig/init-key ::comp [_ opts] opts)
(doseq [c [::comp-a ::comp-b ::comp-c]]
  (derive c ::comp))

(def config {::comp-a [(ig/ref ::comp-b)
                       (ig/ref ::comp-c)]
             ::comp-b 10
             ::comp-c 20})
(def prep (constantly config))


(t/deftest use-ns-local-prep-test
  (sut/with-bulkhead [a ::comp-a]
    {}
    (t/is (= [10 20] a))))
