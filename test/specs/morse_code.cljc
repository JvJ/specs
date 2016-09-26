(ns specs.morse-code
  (:require  #?(:clj [clojure.test :as t]
                :cljs [cljs.test :as t :include-macros true])
             [#?(:clj clojure.core.async :cljs cljs.core.async)
              :as async
              :include-macros true
              :refer [chan go <! >! put! take! go-loop
                      timeout alts! alt! close!]]
             [specs.core :refer :all]
             [specs.ecs :refer :all]
             [specs.control :as ctrl
              :refer [defcontroller *ctrl*]]
             [clojure.core.matrix :as mx]
             [specs.quil-util :as qu]
             (quil [core :as q]
                   [middleware :as qm])))


(defcomponent Pos [x y])
(defcomponent Vel [dx dy])

(defcomponent Colors [edge fill])

(defcomponent Box [width height])
(defcomponent Circle [radius])

(defsys update-v
  [e]
  [p Pos
   {:keys [dx dy]} Vel]
  (-> p
      (update :x + dx)
      (update :y + dy)))

(def render-fns (atom []))

(defsys render-box
  [e]
  [{:keys [x y] :as p} Pos
   {:keys [width height] :as b} Box
   {:keys [edge fill] :as c} Colors]

  (qu/render
   render-fns
   (fn []
     (apply q/fill fill)
     (apply q/stroke edge)
     (q/rect-mode :center)
     (q/rect x y width height)))
  
  e)

(defcomponent Blinker [state])

(defsys render-blinker
  [e]
  [{:keys [x y]} Pos
   {:keys [state]} Blinker]
  
  (let [fill   (if state [255 0 0] [150 150 150])
        stroke [0 0 0]]
    (qu/render
     render-fns
     (fn []
       (apply q/fill fill)
       (apply q/stroke stroke)
       (q/ellipse-mode :center)
       (q/ellipse x y 10 10))))
  
  e)

(defn set-blinker
  "A function that returns another function.
  The second function is used as a message."
  [state]
  (sysfn
   [e]
   [b Blinker]
   (assoc b :state state)))

(defonce blink-chan (chan))

(defcontroller
  blink-times
  []
  (go-loop []
      (let [n (<! blink-chan)]
        (doseq [i (range n)]
          (>! *ctrl* (set-blinker true))
          (<! (timeout 300))
          (>! *ctrl* (set-blinker false))
          (<! (timeout 1000))))))


;;;; LEFTOFF: Make utility functions like "add-component" and stuff

(def entity-list
  [(entity (qu/->RenderContext [100 100 100]
                               render-fns))
   (entity (->Pos 100 100)
           (->Box 80 45)
           (->Colors [0 0 0] [0 150 0])
           (->Blinker false))])

(def current-state (atom nil))
(defn current-control
  []
  (:control @current-state))

(defn start-sketch
  []
  (qu/make-sketch
   :title "Morse Code"
   :framerate 50
   :render-fns render-fns
   :entities entity-list
   :systems [qu/clear-render
             update-v
             render-box
             render-blinker]
   :state-atom current-state))
