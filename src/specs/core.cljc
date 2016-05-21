(ns specs.core
  (:require [#?(:clj clojure.core.async :cljs cljs.core.async)
             :as async
             :include-macros true
             :refer [chan go <! >! put! take! go-loop
                     timeout alts! alt! close!]]
            ;; TODO: CLJS dependencies.  Can :refer-macros
            ;; be used for convenience??? Find out!!
            [cross-map.core :as cmap
             :refer [cross-map cross-cols cross-rows cross
                     cols rows]]
            [cross-map.util :as cmu
             :refer [new-uuid <| |> $ Err kvp pair?]]

            [#?(:clj clj-time.core :cljs cljs-time.core)
             :as tm]
            
            #?(:clj [clojure.tools.macro :as cmt
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

;; Component data types
(defprotocol IComponent
  "Extend this protocol to implement any component type.
  It's easier to use the defcomponent macro to generate
  component types."
  (getCType [this] "Get the component-type of this object."))

(defn ctype
  "Get the type of the object passed-in.
  First tries to return the :type metadata.  Then, if
  the object satisfies IComponent, .getType is called.
  Finally, the object's type is returned if all else fails."
  [o]
  (or (some-> o meta :type)
      (and (satisfies? IComponent o) (.getCType o))
      (type o)))

;; Helper functions for defcomponent
(defn- impl-class-sym
  "Convert a symbol defining a component type to the
  name of its implementation class.  This is done
  by adding 2 underscores before the symbol name.
  Namespace is kept the same."
  [s]
  (symbol (namespace s) (str "__" (name s))))

(defn- ctype-keyword
  "Convert a ctype symbol to a namespace-qualified
  keyword.  If no NS is provided for the symbol,
  the current namespace is used."
  [s]
  (keyword (or (namespace s)
               (name (.-name *ns*)))
           (name s)))

(defn- factory-fn-sym
  "Create a factory function symbol for defcomponent."
  [s]
  (symbol (namespace s) (str "->" (name s))))

(defmacro defcomponent
  "Define a component type.  Use is similar to defrecord, but an optional
  docstring and/or attr-map may be provided."
  {:arglists '([name docstring? attr-map? [& fields] & opts+specs])}
  [nme & args]
  (let [[nme [fields & args]] (name-with-attributes nme args)
        _ (assert (vector? fields) "defcomponent requires vector of fields.")
        nme-kw (ctype-keyword nme)
        impl-class (impl-class-sym nme)
        fac-fn (factory-fn-sym nme)]
    `(do (defrecord ~impl-class
             ~fields
           IComponent
           (~'getCType [~'this] ~nme-kw)
           ~@args)
         (def ~nme ~nme-kw)
         (defn ~fac-fn ~fields
           (new ~impl-class ~@fields)))))

;;; ECS data types
(defn e-map
  "Create an entity map."
  [id & {:as m}]
  (vary-meta m assoc :type ::e-map :eid id))

(defn e-map?
  [m]
  (isa? (ctype m) ::e-map))

(defn get-eid
  "Retrieves the eid from the metadata of an e-map."
  [m]
  (-> m meta :eid))

(defn ->e-map
  "Convert a structure to an entity map."
  [id m]
  (if (and (e-map? m) (= (get-eid m) id))
    m
    (vary-meta m assoc :type ::e-map :eid id)))

(defn c-map
  "Create a component map."
  [ctp & {:as m}]
  (vary-meta m assoc :type ::c-map :ctype ctp))

(defn c-map?
  [m]
  (isa? (ctype m) ::c-map))

(defn get-ctype
  "Retrieves the ctype from the metadata of a c-map."
  [m]
  (-> m meta :ctype))

(defn ->c-map
  "Convert a structure to a component map."
  [ctp m]
  (if (and (c-map? m) (= (get-ctype m) ctp))
    m
    (vary-meta m assoc :type ::c-map :ctype ctp)))


(defn ec-entry
  [k v]
  (with-meta
    [k v]
    {:type ::ec-entry}))

(def ece ec-entry)

(defn ec-entry?
  [kv]
  (= ::ec-entry (ctype kv)))

(defn ec-seq
  "Enter all the return values in here."
  [& args]
  (with-meta args
    {:type ::ec-seq}))

(defn ec-seq?
  [s]
  (= ::ec-seq (ctype s)))

(defn entity-with-id
  "Create a new entity with the specified ID."
  [id & components]
  (->e-map id
           (into {} (map #(kvp (ctype %) %))
                 components)))

(defn new-entity
  "Utility function for easy creation of a new entity."
  [& {:as components}]
  (apply entity-with-id (new-uuid) components))

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
  [o]
  (let [t (ctype o)]
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
          (ec-seq (ece [*eid* t] o))))))

(defn- bind
  "Bindings for match-type profiles.  Returns [profile-form, bind-map]."
  [prof v]
  (let [_ (assert (and (vector? v) (even? (count v)))
                  "bind-ctype requires an even number of forms.") 
        pairs (partition 2 v)
        bind-map (into {} (map vec) pairs)
        kys (map second pairs)
        prof-form `(~prof ~@kys)]
    [prof-form, bind-map]))

(defn- bind-ctype
  [v]
  (bind `match-ctype v))

(defn- bind-eid
  [v]
  (bind `match-eid v))

;;; System functions
(defmacro sysfn
  "Used like fn, but requires a profile before
  parameter declaration.  Multiple arities not supported."
  {:arglists '([name? [params*] profile-or-bindings exprs*])}
  [& args]
  (let [[fn-name [params p-or-b & rest]] (if (symbol? (first args))
                                           [`(~(first args)) (rest args)]
                                           [() args])
        p-or-b (if (vector? p-or-b)
                 `(~'bind-ctype ~p-or-b)
                 p-or-b)
        [prof-form bind-map] (let [[p b & r] p-or-b
                                   _ (assert (and (nil? r)
                                                  (vector? b)
                                                  ('#{bind-ctype bind-eid} p))
                                             (str "Invalid binding form: " p-or-b))]
                               (cond 
                                 (= 'bind-ctype p) (bind-ctype b)
                                 (= 'bind-eid p)   (bind-eid b)))]
    
    `(with-meta
       (fn ~@fn-name [o#]
         (let [~params [o#] ~bind-map o#]
           ~@rest))
       {:profile ~prof-form
        :profile-fn (realize-profile ~prof-form)})))

(defmacro defsys
  "Define a system function.  Syntax similar to defn, but
  profiles/bindings are necessary, just like sysfn.
  Multiple arities not supported."
  {:arglists '([name doc-string? attr-map? [params*] body])}
  [nme & args]
  (let [[nme [params p-or-b & args]] (name-with-attributes nme args)
        p-or-b (if (vector? p-or-b)
                 `(~'bind-ctype ~p-or-b)
                 p-or-b)
        nme (with-meta nme (assoc (meta nme)
                                  :arglists `'(~params ~p-or-b)))]
    `(def ~nme (sysfn ~params ~p-or-b ~@args))))


(defn profile
  "Retrieve the profile data of a sysfn."
  [f]
  (-> f meta :profile))

(defn profile-fn
  "Retrieve the cross-map iteration function
  of a sysfn's profile."
  [f]
  (-> f meta :profile-fn))

(defn preprocess-fn
  "Retrieve the preprocessing function from a sysfn."
  [f]
  (-> f profile-fn meta :preprocess-fn))

(defn env-fn
  "Retrieve the environment creation function from a sysfn."
  [f]
  (-> f profile-fn meta :env-fn))

;;;; Game state iteration

(defn game-state
  "Create a game-state from a collection of entity maps."
  [e-maps]
  (into (cross-map)
        (mapcat (fn [em]
                  (assert (e-map? em) "Inputs to game-state must be e-maps.")
                  (let [eid (get-eid em)]
                    (map #(kvp [eid (key %)] (val %)) em))))
        e-maps))

(defn run-systems
  "Execute the systems provided.  The systems should be in
  an ordered collection (it should satisfy sequential?). "
  [state systems]
  {:pre [(sequential? systems)]}
  (reduce (fn [acc sys]
            (let [enviro-fn (env-fn sys)
                  pre-fn (preprocess-fn sys)
                  prof-fn (profile-fn sys)
                  prof (profile sys)]
              (binding [*state* acc
                        *position* body]
                (into acc
                      (mapcat (fn [kv]
                                (let [{:keys [eid ctype res] :as env}
                                      (enviro-fn kv)]
                                  (binding [*eid* eid
                                            *ctype* ctype]
                                    (-> (pre-fn kv)
                                        (sys)
                                        (coerce-to-ec-seq))))))
                      (prof-fn acc)))))
          state
          systems))

(defn timeout-at
  "Returns a timeout channel that times out at the specified
  time.  t should be a DateTime object, the same type that
  (clj-time.core/now) or (cljs-time.core/now) would return.

  If the current time comes after t, this function returns nil.
  This is useful for signalling skipped-frames."
  [t]
  (let [n (tm/now)]
    (if-let [m (and (tm/before? n t)
                    (tm/in-millis (tm/interval n t)))]
      (timeout m)
      nil)))

(defn beacon
  "Every m milliseconds, x is sent over the channel c.  The default
  value of x is true.  If a function fx is passed in, the sequence
  returned will be (x, (fx x), (fx (fx x)), ...).  (This can be useful
  for numbering frames).  The first value is sent instantaneously, and
  subsequent values follow after timeouts.

  If one of the intervals is missed (i.e. due to long function execution or
  blocked channels), then the value s is sent over the channel.  The default
  value of s is false  If provided, the function fs acts on s the same way
  fx acts on x. (This can be useful for counting skipped frames).

  The beacon may be stopped by sending :stop over c.  Additionally,
  the beacon will stop if one of the values returned by f is created
  with clojure.core/reduced.

  It is suggested that the channel passed-in is buffered, so that frames
  don't get skipped due to waiting for i/o operations on channels."
  ([m c] (beacon m c identity true false))
  ([m c x] (beacon m c identity x false))
  ([m c x s] (beacon m c identity x s))
  ([m c f x s] (beacon m c f x identity s))
  ([m c fx x fs s]
   {:pre [(not (nil? x)), (not (nil? s))]}
   (let [;; To avoid error buildup, we precompute future
         ;; times at exact multiples of m milliseconds from now
         start-time (tm/now)
         frame-times (map (fn [i]
                            (tm/plus start-time
                                     (tm/millis (* (inc i) m))))
                          (range))]
     ;; Start last-x and last-s as nil values to indicate they
     ;; haven't been sent out yet.  It's OK to do this, since
     ;; it is impossible to send nil over a channel.
     (go-loop [last-x nil
               last-s nil
               [ft & ft-rest :as fts] frame-times
               t (timeout-at ft)]
       (if (nil? t)         
         ;; The timeout-at channel couldn't open
         ;; This indicates a skipped frame
         (let [first-time? (nil? last-s)
               f-result (if first-time? nil (fs last-s))
               _ (assert (or first-time?
                             (not (nil? f-result)))
                         (str "fs returned nil with input: " last-s))
               next-s (if first-time? s f-result)]
           (>! c next-s)
           (recur last-x next-s ft-rest
                  (timeout-at ft)))
         ;; Successful timeout-at channel
         (alt!
           c ([v] (if-not (= v :stop)
                    (recur last-x last-s fts t)
                    nil)) ; Terminate the loop by returning nil
           t ([v] (let [first-time? (nil? last-x)
                        f-result (if first-time? nil (fx last-x))
                        _ (assert (or first-time?
                                      (not (nil? f-result)))
                                  (str "fx returned nil with input: " last-x))
                        next-x (if first-time? x f-result)]
                    (>! c next-x)
                    (when-not (reduced? next-x)
                      (let [[f & r] ft-rest]
                        (recur next-x last-s r (timeout-at f))))))))))))

(defn update-loop
  "Async process that updates the system functions based on a
  framerate."
  [framerate]
  ;; LEFTOFF: Do the timers!!!
  (go-loop []))
