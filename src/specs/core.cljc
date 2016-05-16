(ns specs.core
  (:require [clojure.core.async :as async
             :refer [chan go <! >! put! take!]]
            ;; TODO: CLJS dependencies.  Can :refer-macros
            ;; be used for convenience??? Find out!!
            [cross-map.core :as cmap
             :refer [cross-map cross-cols cross-rows cross
                     cols rows]]
            [cross-map.util :as u
             :refer [new-uuid <| |> $ Err kvp pair?]]
            #?(:clj [clojure.tools.macro :as ctm
                     :refer [name-with-attributes]])))

;;;; Constants, for utility
(def ^:const head ::head)
(def ^:const body ::body)
(def ^:const tail ::tail)

;;;; Global Context
(def ^:dynamic *state*
  "The current value of the game state, as it was
  at the beginning of this system's execution."
  nil)

(def ^:dynamic *controller*
  "The controller instance that sent this message,
  if this function is a control message."
  nil)

(def ^:dynamic *position*
  "This may be either :specs.core/head, :specs.core/body,
  or :specs.core/tail.  Head and tail are for control 
  messages executed before or after the main update cycle.
  Body is for synchronous system functions executed in the
  update cycle."
  nil)

(def ^:dynamic ^double *dt*
  "The time difference between the beginning of this
  update and the beginning of the previous one."
  0.0)

;;;; Coerction Context
(def ^:dynamic *profile*
  "The profile of the current system function."
  nil)

(def ^:dynamic *eid*
  "The ID of the entity currently being accessed,
  if any."
  nil)

(def ^:dynamic *ctype*
  "The type of the component currently being accessed,
  if any."
  nil)

;;;; Cross-map iteration profiles

(defn match-ctype
  "Match with each entity that has all specified
  ctypes."
  [& ctypes]
  {:col-keys ctypes
   :every-col :every-col})

;; LEFTOFF: Come up with a decent macro binding system
;; (defmacro bind-ctype)

(defn match-eid
  "Match with each component column that has all
  specified entity ID's."
  [& eids]
  {:row-keys eids
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
  [[ctype ccol]]
  (->EnvSettings nil ctype ccol))

(defn- for-ctype-env
  [[ctype ccol]]
  (->EnvSettings nil ctype ccol))

(defn- for-eid-env
  [[eid e]]
  (->EnvSettings eid nil e))

(defn- for-entry-env
  "A fucntion to return global environment settings based on
  the profile and input."
  [[[id ctype :as kv] cpt]]
  (->EnvSettings id ctype kv))

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
          (with-meta #(apply cross % row-keys col-keys opts) {:env-fn for-entry-env})
          row-keys (if every-row
                     (with-meta #(apply cross-rows % row-keys opts) {:env-fn match-eid-env})
                     (with-meta (fn [cm] (map #(find (rows cm) %) row-keys)) {:env-fn for-eid-env}))
          col-keys (if every-col
                     (with-meta #(apply cross-cols % col-keys opts) {:env-fn match-ctype-env})
                     (with-meta (fn [cm] (map #(find (cols cm) %) col-keys)) {:env-fn for-ctype-env})))))

;;; ECS data types

(defn e-map
  "Create an entity map."
  [& {:as m}]
  (with-meta
    m
    (assoc (meta m) :type ::e-map)))

(defn e-map?
  [m]
  (= ::e-map (type m)))

(defn c-map
  [& {:as m}]
  (with-meta
    m
    (assoc (meta m) :type ::c-map)))

(defn c-map?
  [m]
  (= ::c-map (type m)))

(defn ec-entry
  [k v]
  (with-meta
    [k v]
    {:type ::ec-entry}))

(def ece ec-entry)

(defn ec-entry?
  [kv]
  (= ::ec-entry (type kv)))

(defn ec-seq
  "Enter all the return values in here."
  [& args]
  (with-meta args
    {:type ::ec-seq}))

(defn ec-seq?
  [s]
  (= ::ec-seq (type s)))

;;;; Special operations that can be performed
(defn state-op
  "Turn a function into a state operation."
  [f]
  (with-meta
    f
    {:type ::state-op}))

(defn coerce-to-ec-seq
  "Coerce the provided args to a single ec-seq containing
  either ec-entries or state-ops"
  [& args]
  (mapcat (fn [o]
            (let [t (type o)]
              (case t
                 ::e-map    (do (assert *eid* "Cannot coerce from e-map in this context.")
                                  (->> o
                                       (map (fn [[k v]] (ece [*eid* k] v)))
                                       (apply ec-seq)))
                 ::c-map    (do (assert *ctype* "Cannot coerce from c-map in this context.")
                                  (->> o
                                       (map (fn [[k v]] (ece [k *ctype*] v)))
                                       (apply ec-seq)))
                 ::ec-entry (ec-seq o)
                 ::ec-seq   o
                 ::state-op o
                
                ;; Default case treats it as component, with type as a key
                 (do (assert *eid* "Cannot coerce from single component in this context.")
                     (ec-seq (ece [*eid* t] o))))))))

;;; System functions
(defmacro sysfn
  "Used like fn, but requires a profile before
  parameter declaration.  Multiple arities not supported."
  {:arglists '([name? (profile? [bindings*]) [params*] exprs*])}
  [& args]
  (let [[fn-name [p-and-b & rest]] (if (symbol? (first args))
                                     [`(~(first args)) (rest args)]
                                     [() args])

        [profile bindings] (if (even? (count p-and-b))
                             [nil p-and-b]
                             [(first p-and-b) (rest p-and-b)])

        binding-vec  (into [] (<| (partition-all 2) cat) bindings)
        binding-keys (into [] (<| (partition-all 2) second) bindings)
        
        profile (if profile
                  profile
                  `(match-ctype ~@binding-keys))
        
        ev-prof (eval profile)]
    
    `(with-meta
       (fn ~@fn-name ~@rest)
       {:profile ~profile
        :profile-fn (realize-profile ~profile)})))

;; (defmacro defsys)

