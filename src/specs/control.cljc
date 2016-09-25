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
     control-notify
     beacon
     beacon-notify
     systems
     controllers])

(defn control-state
  "Create a control state.
  The controllers field of the resulting state will be a
  vector with control as its first element.
  
  The parameter :beacon may be a number or a channel.
  If it is a number, it creates a beacon channel with a period of that
  many milliseconds."
  [& {:keys [c-map control beacon systems controllers]
      :or {c-map (cross-map)
           control (chan)
           beacon 20
           systems []
           controllers #{}}}]
  (->ControlState c-map
                  control
                  (chan (async/dropping-buffer 1024))
                  (if (number? beacon)
                    (specs.channels/beacon beacon)
                    beacon)
                  (chan (async/dropping-buffer 1024))
                  systems
                  controllers))

(defn close-control-state!
  "Close all the channels in the control state."
  [{:keys [c-map
           control
           control-notify
           beacon
           beacon-notify
           systems
           controllers]
    :as c-state}]
  (async/close! control)
  (async/close! control-notify)
  (async/close! beacon)
  (async/close! beacon-notify)
  c-state)

;;;; Control messages are functions that execute on control states
;;;; Here are a few utility-functions
(defn add-controller
  [ctrlr]
  (fn [c-state]
    (update c-state :controllers conj ctrlr)))


