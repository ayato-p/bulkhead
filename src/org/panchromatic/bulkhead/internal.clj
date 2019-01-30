(ns org.panchromatic.bulkhead.internal
  (:require [integrant.core :as ig]
            [org.panchromatic.bulkhead.state :as state]))

(defn- parse-bindings
  ([a] (parse-bindings a []))
  ([a b]
   (if (empty? a)
     (into {} b)
     (recur (subvec a 2)
            ((fnil conj []) b (subvec a 0 2))))))

(defn parse-args [args env]
  (letfn [(bindings? [x] (and (vector? x) (even? (count x))))
          (resolvable? [x] (or (contains? env x) (some-> (resolve x) bound?)))
          (mock-map? [x] (or (map? x) (and (symbol? x) (resolvable? x))))]
    (let [bindings (-> (if (bindings? (first args)) (first args) [])
                       parse-bindings)
          args (cond-> args (vector? (first args)) next)
          mock-map (if (mock-map? (first args)) (first args) {})
          body (cond-> args (mock-map? (first args)) next)]
      [bindings mock-map body])))

(defn detect-prep-fn [env]
  (if (or (contains? env 'prep)
          (some-> (resolve 'prep) bound?))
    'prep
    `state/prep))

(defn- normalize-key [k]
  (if (vector? k) (ig/composite-keyword k) k))

(defn- make-new-key [k]
  (keyword (namespace k) (str (gensym (str (name k) "_")))))

(defn make-keypairs [org-keys]
  (zipmap org-keys
          (->> (map normalize-key org-keys)
               (map make-new-key))))

(defn create-mock-components! [org&mock-keypairs mock-map]
  (doseq [[o m] org&mock-keypairs
          :let [f (get mock-map o)
                p (normalize-key o)]]
    (derive m p)
    (.addMethod ig/init-key m f)
    (.addMethod ig/halt-key! m (constantly m))))

(defn bind-components [system-sym bindings]
  (letfn [(f [k]
            (let [k' (normalize-key k)]
              `(val (ig/find-derived-1 ~system-sym ~k'))))]
    (reduce-kv (fn [v s k]
                 (cond-> v
                   (not= s '_) (conj s (f k))))
               []
               bindings)))

(defn- keywords-table []
  (let [^java.lang.reflect.Field field (.getDeclaredField clojure.lang.Keyword "table")]
    (.setAccessible field true)
    (.get field nil)))

(defn- unintern-keyword [kw]
  (let [sym (if (qualified-keyword? kw)
              (symbol (namespace kw) (name kw))
              (symbol (name kw)))]
    (.remove (keywords-table) sym)))

(defn destroy-mock-components! [org&mock-keypairs]
  (doseq [[o m] org&mock-keypairs
          :let [p (normalize-key o)]]
    (underive m p)
    (remove-method ig/init-key m)
    (remove-method ig/halt-key! m)
    (unintern-keyword m)))
