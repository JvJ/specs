(ns specs.control
  (:require
   [#?(:clj clojure.core.async :cljs cljs.core.async)
    :as async
    :include-macros true
    :refer [chan go <! >! put! take! go-loop
            timeout alts! alt! close!]]
   [cross-map.core :as cmap
             :refer [cross-map cross-cols cross-rows cross
                     cols rows]]
   [specs.channels :as s-ch
             :refer [beacon]]))

(defrecord
    ^{:doc "A process and i/o channel for sending system functions to the
main update loop."}
    Controller
    [channel
     process])

(defn controller
  "Create a controller with a channel.
  If a channel is not provided, it defaults to (chan)."
  ([process] (controller (chan) process))
  ([channel process]
   (->Controller channel process)))

(defrecord
    ^{:doc "This structure represents the full game-state cross-map along with
the control channels, system order, and controllers."}
    ControlState
    [c-map
     control
     beacon
     systems
     controllers])

(defn control-state
  "Create a control state.
  The controllers field of the resulting state will be a
  vector with control as its first element."
  [& {:keys [c-map control frame-time systems controllers]
      :or {c-map (cross-map)
           control (chan)
           frame-time 20
           systems []
           controllers #{}}}]
  (->ControlState c-map
                  control
                  (beacon frame-time)
                  systems
                  controllers))

;;;; Control messages are functions that execute on control states
;;;; Here are a few utility-functions
(defn add-controller
  [ctrlr]
  (fn [c-state]
    (update c-state :controllers conj ctrlr)))
