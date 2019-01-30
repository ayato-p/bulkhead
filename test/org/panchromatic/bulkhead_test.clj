(ns org.panchromatic.bulkhead-test
  (:require [clojure.test :as t]
            [integrant.core :as ig]
            [org.panchromatic.bulkhead :as sut]))

(defmethod ig/init-key ::comp [_ opts] opts)

(doseq [c [::comp-a ::comp-b ::comp-c]]
  (derive c ::comp)
  (doseq [n (range 3)]
    (-> (namespace c)
        (keyword (str (name c) n))
        (derive c))))

(defn- create-mock-fn [& _]
  "This is mock")

(t/deftest simple-mocking-test
  (t/testing "simply mocking"
    (let [config {::comp-a (ig/ref ::comp-b)
                  ::comp-b (ig/ref ::comp-c)
                  ::comp-c :comp-c}
          prep (constantly config)]

      (sut/with-bulkhead [a ::comp-a]
        {::comp-b (constantly :mock-b)}
        (t/is (= :mock-b a))
        (t/is (false? (contains? $system :comp-c))))))

  (t/testing "use symbol for mock"
    (let [prep (constantly {::comp-a 1})]
      (sut/with-bulkhead [a ::comp-a]
        {::comp-a create-mock-fn}
        (= "This is mock" a))))

  (t/testing "bind component to underscore just ignore, but should start component"
    (let [config {::comp-a (ig/ref ::comp-c)
                  ::comp-b :comp-b
                  ::comp-c :comp-c}
          prep (constantly config)]

      (sut/with-bulkhead [a ::comp-a
                          _ ::comp-b]
        {}
        (t/is (= (set (keys $system)) #{::comp-a ::comp-b ::comp-c}))))

    (let [config {::comp-a (ig/refset ::comp-b)
                  ::comp-b1 10
                  ::comp-b2 20}
          prep (constantly config)]

      (sut/with-bulkhead [a ::comp-a
                          _ ::comp-b]
        {}
        (t/is (= a #{10 20}))))

    (let [config {::comp-a (ig/refset ::comp-b)
                  ::comp-b1 10
                  ::comp-b2 20}
          prep (constantly config)]

      (sut/with-bulkhead [a ::comp-a
                          _ ::comp-b]
        {}
        (t/is (= a #{10 20}))))

    (let [config {::comp-a (ig/refset ::comp-b)
                  ::comp-b1 10
                  ::comp-b2 20}
          prep (constantly config)]

      (sut/with-bulkhead [a ::comp-a
                          _ ::comp-b]
        {::comp-b2 (constantly 42)}
        (t/is (= a #{10 42}))))))

(doseq [c [::comp-d ::comp-e ::comp-f]]
  (derive c ::comp))

(t/deftest compsite-key-test
  (let [config {::comp-d (ig/refset ::comp-e)
                [::comp-e ::comp-e1] 10
                [::comp-e ::comp-e2] 20}
        prep (constantly config)]

    (sut/with-bulkhead [d ::comp-d
                        _ ::comp-e]
      {[::comp-e ::comp-e2] (constantly 30)
       [::comp-e ::comp-e1] (constantly 12)}
      (t/is (= #{12 30} d)))

    (sut/with-bulkhead [d ::comp-d
                        _ ::comp-e]
      {}
      (t/is (= #{10 20} d)))

    (t/testing "just start ::comp-d's child"
      (sut/with-bulkhead [d ::comp-d
                          _ ::comp-e]
        {::comp-e (constantly 30)}
        (t/is (= #{30} d))))))

(defprotocol MyProtocol
  (proto-fn [this]))

(defmethod ig/init-key ::proto [_ opts]
  (reify MyProtocol
    (proto-fn [_] opts)))

(doseq [c [::proto-a ::proto-b ::proto-c]]
  (derive c ::proto))

(t/deftest mock-with-protocol-test
  (let [config {::proto-a [(ig/ref ::proto-b)
                           (ig/ref ::proto-c)]
                ::proto-b nil
                ::proto-c nil}
        prep (constantly config)]
    (sut/with-bulkhead [a ::proto-a]
      {::proto-b (constantly
                  (reify MyProtocol
                    (proto-fn [_] 30)))
       ::proto-c (constantly
                  (reify MyProtocol
                    (proto-fn [_] 12)))}
      (t/is (every? (partial satisfies? MyProtocol) (proto-fn a)))
      (t/is
       (->> (proto-fn a)
            (map proto-fn)
            (reduce +)
            (= 42))))))
