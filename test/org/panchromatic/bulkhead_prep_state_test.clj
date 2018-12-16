(ns org.panchromatic.bulkhead-prep-state-test
  (:require [integrant.core :as ig]
            [org.panchromatic.bulkhead :as sut]
            [clojure.test :as t]))

(defmethod ig/init-key ::comp [_ opts] opts)
(doseq [c [::comp-a ::comp-b ::comp-c]]
  (derive c ::comp))

(let [config {::comp-a [(ig/ref ::comp-b)
                        (ig/ref ::comp-c)]
              ::comp-b 10
              ::comp-c 20}]
  (sut/set-prep! (constantly config)))

(t/deftest use-prep-state-test
  (sut/with-bulkhead [a ::comp-a]
    {}
    (t/is (= [10 20] a))))
