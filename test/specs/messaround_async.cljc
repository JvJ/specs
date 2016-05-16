(ns specs.messaround-async
  "A namespace for experimenting with asyn functions."
  (:require [clojure.core.async :as async
             :refer [go go-loop put! take!
                     <! >! alts! timeout
                     chan buffer]]
   #?(:clj [clojure.test :as t]
                :cljs [cljs.test :as t :include-macros true])))

(defn push-nums
  "Push a few numbers on a channel."
  [n c]
  (go (loop [i 0]
        (when (< i n)
          (>! c i)
          (recur (inc i))))
      (>! c :end)))

(defn print-nums
  "Take the numbers from the channel and print them."
  [c]
  (go
    (loop []
        (let [n (<! c)]
          (when (not= :end n)
            (println "A num: " n)
            (recur))))
    (println "Finished with nums!!")))


(defn buf-test-push
  [c]
  (go (println "Pushing 1")
      (>! c 1)
      (println "Pushing 2")
      (>! c 2)
      (println "Done")))

(defn buf-test-pull
  [c]
  (go (println "Pulled first: "(<! c))
      (println "Pulled second: " (<! c))
      (println "End")))


(defn test-mapcat
  [o]
  (if (integer? o)
    (range o)
    ()))
