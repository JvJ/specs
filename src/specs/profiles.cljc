(ns specs.profiles
  (:require [specs.ecs :refer :all]
            [cross-map.core :refer [cross-map cross-cols cross-rows cross
                                    cols rows]]))

;;;; Cross-map iteration profiles

(defn match-ctype
  "Match with each entity that has all specified
  ctypes."
  [& ctypes]
  {:col-keys (or ctypes ())
   :every-col :every-col})

(defn match-eid
  "Match with each component column that has all
  specified entity ID's."
  [& eids]
  {:row-keys (or eids ())
   :every-row :every-row})

(defn for-ctype
  "Match with the component column of each ctype
  specified."
  [& ctypes]
  {:col-keys ctypes
   :any-col :any-col})

(defn for-eid
  "Match with the eid row of each eid specified."
  [& eids]
  {:row-keys eids
   :any-row :any-row})

(defn for-entry
  "Compose other profiles.  Using this profile will
  result in iteration over individual ec-entries."
  [& profiles] 
  (let [res (apply merge-with
                   (fn [v1 v2]
                     (if (and (seq? v1) (seq? v2))
                       (concat v1 v2)
                       v2))
                   profiles)]
    (assert (not (and (:any-row res) (:every-row res)))
            "Invalid profile: Cannot specify both :any-row and :every-row.")
    (assert (not (and (:any-col res) (:every-col res)))
            "Invalid profile: Cannot specify both :any-col and :every-col.")
    res))

(defrecord EnvSettings
    [eid ctype res])

(defn- match-ctype-env
  [[id e]]
  (->EnvSettings id nil e))

(defn- match-eid-env
  [[ctp ccol]]
  (->EnvSettings nil ctp ccol))

(defn- for-ctype-env
  [[ctp ccol]]
  (->EnvSettings nil ctp ccol))

(defn- for-eid-env
  [[eid e]]
  (->EnvSettings eid nil e))

(defn- for-entry-env
  "A fucntion to return global environment settings based on
  the profile and input."
  [[[id ctype :as kv] _]]
  (->EnvSettings id ctype kv))

(defn- match-ctype-preprocess
  [[eid e]]
  (->e-map eid e))

(defn- match-eid-preprocess
  [[_ ccol]]
  (->c-map ccol))

(defn- for-ctype-preprocess
  [[ctp ccol]]
  (->c-map ctype ccol))

(defn- for-eid-preprocess
  [[eid e]]
  (->e-map eid e))

(defn- for-entry-preprocess
  [[[id ctp :as kv] _]]
  (ec-entry id ctp))

(defn realize-profile
  "Takes the profile specified and produces a
  function that can be called on a cross-map
  to iterate over the specified entries."
  [{:keys [row-keys col-keys any-row any-col every-row every-col]
    :as profile}]
  (assert (not (and any-row every-row))
          "Invalid profile: Cannot specify both :any-row and :every-row.")
  (assert (not (and any-col every-col))
          "Invalid profile: Cannot specify both :any-col and :every-col.")
  (assert (or row-keys col-keys)
          "Invalid profile: Either row-keys or col-keys must be specified.")
  (let [opts (filter identity [any-row every-row any-col every-col])]
    (cond (and row-keys col-keys)
          (with-meta #(apply cross % row-keys col-keys opts) {:env-fn for-entry-env
                                                              :preprocess-fn for-entry-preprocess})
          row-keys (if every-row
                     (with-meta #(apply cross-rows % row-keys opts) {:env-fn match-eid-env
                                                                     :preprocess-fn match-eid-preprocess})
                     (with-meta (fn [cm] (map #(find (rows cm) %) row-keys)) {:env-fn for-eid-env
                                                                              :preprocess-fn for-eid-preprocess}))
          col-keys (if every-col
                     (with-meta #(apply cross-cols % col-keys opts) {:env-fn match-ctype-env
                                                                     :preprocess-fn match-ctype-preprocess})
                     (with-meta (fn [cm] (map #(find (cols cm) %) col-keys)) {:env-fn for-ctype-env
                                                                              :preprocess-fn for-ctype-preprocess})))))
