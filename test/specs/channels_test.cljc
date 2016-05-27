(ns specs.channels-test
  (:require [#?(:clj clojure.core.async :cljs cljs.core.async)
             :as async
             :include-macros true
             :refer [chan go <! >! put! take! go-loop
                     timeout alts! alt! close! <!! >!!]]
            [specs.channels :refer :all]
            #?(:clj [clojure.test :as t :refer :all]
               :cljs [cljs.test :as t :include-macros true])
            [#?(:clj clj-time.core :cljs cljs-time.core)
             :as tm]))


;;;; Timeout-at and beacon testing

(def err-thresh 1.0)
(def num-samples 10)
(def max-delay-millis 60000)

(deftest
  ^{:doc "Randomly generate a set of future points in time, create
tieout-at channels for them, and make sure the mean absolute deviation
is less than the specified threshold."}
  timeout-at-test

  ;;; First, generate a bunch of future times, between 0 and max-delay-millis
  ;;; from now.
  (let [now (tm/now)
        times (repeatedly num-samples #(tm/plus now (tm/millis (rand-int max-delay-millis))))
        ;; Using mapv to ensure the chans are evaluated beforehand
        chans (mapv (fn [t] [t (timeout-at t)]) times)

        processes (mapv (fn [[t c]]
                          (go
                            (<! c)
                            [t (tm/now)]))
                        chans)

        num-procs (count processes)
        
        ;; The final computation
        errors (<!! (go-loop [nm 0
                              coll []]
                      (if (< nm num-procs)
                        (let [[[t t'] _] (alts! processes)]
                          (recur (inc nm)
                                 (conj coll (->> (tm/interval t t')
                                                 (tm/in-millis)))))
                        coll)))

        avg-err (/ (float (reduce + errors)) (float (count errors)))
        _ (println "Avg error: " avg-err "ms.")]

    (is (< avg-err
           err-thresh))))

(def min-delay-ms 2)
(def target-delay-ms 20)
(def max-delay-ms 22)
(def min-max 1/10)
(def num-frames 1000)
(def num-samples 10)

;; LEFTOFF: This isn't working perfectly, but close enough for now!!
(deftest
  ^{:doc "Test the beacon function to ensure that we get the
expected number of successful/skipped frames."}
  beacon-test
  (let [start-time (tm/now)
        frame-seq (map #(tm/plus start-time 
                                 (tm/millis (* (inc %) target-delay-ms)))
                       (range))
        filters (mapv (fn [[in frame-seq]]
                        (let [out (chan)]
                          (go-loop [n 0
                                    [f & fs] frame-seq]
                            (if (>= n num-frames) (do (close! in) 
                                                      (close! out))
                                (let [dly (+ min-delay-ms
                                             (rand-int
                                              (- max-delay-ms
                                                 min-delay-ms)))
                                      _ (<! (timeout-at
                                             (tm/plus f (tm/millis dly))))
                                      v (<! in)]
                                  (if (nil? v)
                                    (close! out)
                                    (do (>! out v)
                                        (recur (inc n) fs))))))
                          ;; Return the output channel
                          out))
                      (repeatedly num-samples #(-> [(beacon target-delay-ms)
                                                    frame-seq])))
        
        merged (async/merge filters (* num-samples num-frames))

        
        data-proc (go-loop [hits 0 misses 0]
                    (let [v (<! merged)]
                      (cond (nil? v) {:hits hits :misses misses}
                            (true? v) (recur (inc hits) misses)
                            (false? v)  (recur hits (inc misses))
                            :else (do 
                                    (assert false
                                            (str "Unexpected value received: " v))))))

        {:keys [hits misses]} (<!! data-proc)]

    (println "Hits & misses:" [hits misses])))
