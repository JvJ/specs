(ns specs.channels
  "Implementing custom channels and channel-related functions.
  Beacon channels are defined here."
  (:require [#?(:clj clojure.core.async :cljs cljs.core.async)
             :as async
             :include-macros true
             :refer [chan go <! >! put! take! go-loop
                     timeout alts! alt! close!]]
            [#?(:clj clojure.core.async.impl.protocols
                :cljs clojure.core.async.impl.protocols)
             :as p]
            
            [#?(:clj clj-time.core :cljs cljs-time.core)
             :as tm]

            [specs.profiles :refer :all]))


(defn timeout-at
  "Returns a timeout channel that times out at the specified
  time.  t should be a DateTime object, the same type that
  (clj-time.core/now) or (cljs-time.core/now) would return."
  [t]
  (let [n (tm/now)]
    (if (tm/before? n t)
      (timeout (tm/in-millis (tm/interval n t)))
      (timeout 0))))

(defn secf
  "A double representing the current second and its milli component."
  [t]
  (+ (tm/second t) (/ (tm/milli t) 1000.0)))

;; TODO: Make adjustable beacon!

(defn beacon
  "Create a channel that emits a value every m milliseconds.
  
  More specifically, it emits values at pre-calculated time points at exact
  multiples of m milliseconds from the time of creation.
  
  For a given time point, if the value is read from the channel before the
  next time point, this counts as a success.  If it is read after the next
  time point, it is a failure.  True means success, false means failure.

  Closing the channel stops the beacon process."
  ([m]
   (let [start-time (tm/now)
         c1 (chan)
         c2 (chan)
         frame-seq (map #(tm/plus start-time
                                  (tm/millis (* (inc %) m)))
                        (range))]
     ;; The Emitter
     (go
       (>! c1 (first frame-seq))
       (loop [[frm1 & [frm2 :as rest-frms] :as frms] frame-seq]
         (alt!
           c1 ([v]
               (if-not (nil? v)
                 (recur frms)))
           (timeout-at frm1) ([_]
                              (>! c1 frm2)
                              (recur rest-frms)))))
     ;; The filter
     (go-loop []
       (alt!
         c1 ([v]
             (>! c2 (tm/before? (tm/now) v))
             (recur))
         c2 ([v]
             (if (nil? v)
               (close! c1)
               (recur)))))
     ;; Return the channel
     c2)))

(defprotocol IProfileChannel
  "A channel with an associated cross-map selector."
  (profile [this] "Get the unrealized cross-map profile for this channel.")
  (profile-fn [this] "Get the fully realized cross-map profile for this channel."))

(defrecord
    ProfileChannel
    [channel
     prof
     prof-fn]

  IProfileChannel
  (profile [this] prof)
  (profile-fn [this] prof-fn)
  
  p/ReadPort
  (take! [this handler]
    (p/take! channel handler))

  p/WritePort
  (put! [this msg handler]
    (p/put! channel msg handler))

  p/Channel
  (close! [this]
    (p/close! channel)))


(defn profile-chan
  "Create a profile-channel with a realized profile.
  Optional additional args are passed to core.async/chan."
  [prof-fn & args]
  (->ProfileChannel (apply chan args) prof-fn))

(defn pchan
  "Construct a profile channel with an unrealized profile.
  Optional additional args are passwd to core.async/chan."
  [prof & args]
  (apply profile-chan (realize-profile prof) args))
