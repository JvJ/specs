(ns specs.quil-util
  (:require [#?(:clj clojure.core.async :cljs cljs.core.async)
             :as async
             :include-macros true
             :refer [chan go <! >! put! take! go-loop
                     timeout alts! alt! close!]]
            [specs.util :as sut]
            [specs.core :refer :all]
            [specs.control :as ctrl]
            [specs.ecs :refer :all]
            #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])
            (quil [core :as q]
                   [middleware :as qm])))


;;;; A rendering context.  Cleared each frame.
;;;; Functions pushed here will
(defcomponent RenderContext
  "Should be added as a single global to the ecs."
  [bg-color
   render-fn-atom])

(defsys clear-render
  "Clears the rendering system once per frame."
  [e]
  [{:keys [bg-color render-fn-atom]} RenderContext]
  (reset! render-fn-atom [#(apply q/background bg-color)]))

(defn render [fn-atom fn]
  (swap! fn-atom conj fn))

(defn make-sketch
  "Helper function to make it easy to make a sketch."
  [& {:keys [title
             size
             systems
             entities
             framerate
             render-fns
             beacon
             state-atom]
      :or {framerate 50
           beacon 20
           size [640 480]
           state-atom (atom nil)}}]

  (letfn [(setup []
            (q/frame-rate framerate)
            (q/color-mode :rgb)
            (let [ret (ctrl/control-state
                       :systems systems
                       :c-map (game-state entities)
                       :beacon beacon)]
              (update-loop ret)
              (reset! state-atom ret)))
          
          (draw [state]
            (doseq [f @render-fns]
              (f))
            (reset! state-atom state))]
    
    (q/sketch
     :title title
     :size size
     :update identity
     :setup setup
     :draw draw
     :features [:no-start]
     :middleware [qm/fun-mode]
     :on-close (fn [] (put! (:control @state-atom) (constantly nil))))))


