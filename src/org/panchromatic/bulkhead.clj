(ns org.panchromatic.bulkhead
  (:require [clojure.spec.alpha :as s]
            [integrant.core :as ig]
            [org.panchromatic.bulkhead.spec :as spec]
            [org.panchromatic.bulkhead.state :as state]))

(defn set-prep! [f]
  (alter-var-root #'state/prep (constantly f)))

(defn- normalize-key [k]
  (if (vector? k) (ig/composite-keyword k) k))

(defn- make-new-key [k]
  (keyword (namespace k) (str (gensym (str (name k) "_")))))

(defn- make-keypairs [org-keys]
  (zipmap org-keys
          (->> (map normalize-key org-keys)
               (map make-new-key))))

(defn- generate-mock-components [org&mock-keypairs mock-map]
  (letfn [(generate [parent-key mock-key create-mock-fn]
            `((derive ~mock-key ~parent-key)
              (defmethod ig/init-key ~mock-key [k# v#]
                (~create-mock-fn k# v#))))]
    (apply concat
           (for [[o m] org&mock-keypairs
                 :let [f (get mock-map o)
                       p (normalize-key o)]]
             (generate p m f)))))

(defn- destroy-mock-components [org&mock-keypairs]
  (letfn [(destroy [parent-key mock-key]
            `((underive ~mock-key ~parent-key)
              (remove-method ig/init-key ~mock-key)))]
    (apply concat
           (for [[o m] org&mock-keypairs
                 :let [p (normalize-key o)]]
             (destroy p m)))))

(defn- bind-components [system-sym bindings]
  (letfn [(f [k]
            (let [k' (normalize-key k)]
              `(val (ig/find-derived-1 ~system-sym ~k'))))]
    (reduce-kv (fn [v s k]
                 (cond-> v
                   (not= s '_) (conj s (f k))))
               []
               bindings)))

(defn- detect-prep-fn [env]
  (if (or (contains? env 'prep)
          (some-> (resolve 'prep) bound?))
    'prep
    `state/prep))

(defmacro
  with-bulkhead
  {:arglists '([bindings mock-map body])}
  [& args]
  (let [{:keys [bindings mock-map body] :as parsed}
        (s/conform ::spec/with-bulkhead-args args)]
    (when (= ::s/invalid parsed)
      (spec/throw-spec-error ::spec/with-bulkhead-args args))
    (let [bindings (->> (map (juxt :sym :key) bindings)
                        (into {}))
          org&mock-keypairs (make-keypairs (keys mock-map))
          startable-keys (-> (reduce-kv #(-> (disj %1 %2) (conj %3))
                                        (set (vals bindings))
                                        org&mock-keypairs)
                             vec)
          prep (detect-prep-fn &env)]
      `(do
         ~@(generate-mock-components org&mock-keypairs mock-map)
         (try
           (let [~'$system (-> (reduce-kv #(-> (dissoc %1 %2) (assoc %3 {}))
                                          (~prep)
                                          ~org&mock-keypairs)
                               (ig/init ~startable-keys))
                 ~@(bind-components '$system bindings)]
             (try
               ~@body
               (finally
                 (ig/halt! ~'$system ~startable-keys))))
           (finally
             ~@(destroy-mock-components org&mock-keypairs)))))))
