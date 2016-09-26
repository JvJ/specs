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

            [specs.ecs :refer :all]

            [specs.profiles :refer :all]
            
            [specs.channels :as s-ch
             :refer [beacon IProfileChannel profile-chan pchan]]
            [specs.control :as s-ctrl
             :refer [controller control-state close-control-state!]]

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
      
      ;; Default casecontr treats it as component, with type as a key
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
       {:type ::Sysfn
        :profile ~prof-form
        :profile-fn (realize-profile ~prof-form)})))

(defn sysfn?
  "Determine whether or not an object is a sysfn."
  [f]
  (= (type f) ::Sysfn))

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
    `(def ~nme (sysfn ~nme ~params ~p-or-b ~@args))))


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

;;;; Game state

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

(defn update-loop
  "Async process that updates the system functions.

  It requires an initial game state, and returns a control state.

  Passing functions of c-state to c-state over the command channel
  causes the c-state to be updated.  If a function returns
  a nil value for a c-state, the loop will exit.

  A skip handler may be provided, which is a function from c-state
  to c-state."
  ([control-state]
   (update-loop control-state identity))
  ([control-state skip-handler]
   (let [c-state control-state]
     (go-loop [{:keys [c-map
                       control
                       control-notify
                       beacon
                       beacon-notify
                       systems
                       controllers]
                :as state} c-state
               last-state c-state
               channels [control beacon]]
       (let [;; Pre-loop setup
             same-control     (= control
                                 (:control last-state))
             same-controllers (= controllers
                                 (:controllers last-state))
             channels (cond (not same-controllers) (into [control beacon]
                                                         (map :control-channel)
                                                         controllers)
                            (not same-control) (assoc channels
                                                      0 control
                                                      1 beacon)
                            :else channels)
             ;; Waiting for operations
             [value port] (try (alts! channels)
                               (catch Exception e
                                 (assert false (str "Alt error, channels: " channels ", msg: " e))))

             ;; TODO: Bind the appropriate globals!
             
             next-state (cond
                          ;; Sysfns get applied to the state
                          (sysfn? value)
                          (do (println "Sysfn updating...")
                              (update state :c-map run-systems [value]))
                          
                          ;; Control channel
                          (= port control)
                          (do (assert (fn? value)
                                      (str "Control channel requires function messages.  Received: " value))
                              (value state))
                          
                          ;; Beacon
                          (= port beacon)
                          (do (if value
                                (update state :c-map run-systems systems)
                                (skip-handler state)))


                          
                          ;; Everything else
                          :else
                          (do (assert (fn? value)
                                      (str "Controller channel requires function messages.  Channels: " channels "Value:" (or value "nil") ", Port: " (or port "nil")))
                              (value state)))

             ;; Push messages onto the notification channels if necessary
             ;; TODO: What kind of notification channel is used for controller messages?
             _ (if next-state
                 (cond (= port control) (put! control-notify next-state)
                       (= port beacon) (put! beacon-notify next-state)))]
         (if next-state
           (recur next-state
                  state
                  channels)
           ;; Else
           (do (close-control-state! next-state)))))
     
     ;; Return the control channel
     (:control c-state))))

;; TODO: Change "control" to "command" to make things a little less confusing
;; TODO: Implement a special instance of vector that has the control and beacon
;; accessible???
