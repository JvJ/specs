(ns specs.game-test
  (:require  #?(:clj [clojure.test :as t]
                :cljs [cljs.test :as t :include-macros true])
             [#?(:clj clojure.core.async :cljs cljs.core.async)
              :as async
              :include-macros true
              :refer [chan go <! >! put! take! go-loop
                      timeout alts! alt! close!]]
             [specs.core :refer :all]
             [specs.ecs :refer :all]
             [specs.control :as ctrl]
             [clojure.core.matrix :as mx]
             (quil [core :as q]
                   [middleware :as qm])))

;;;; A test, using quil


;;;; Some global timer functions
(def last-frame-millis (atom 0))
(defn delta-t []
  (- (q/millis) @last-frame-millis))

;;;; Physical component
(defcomponent Phys
  "The physical attributes of this entity."
  [pos vel rot rot-vel])


(defsys phys-update
  "Update the rotation and position."
  [e]
  [p Phys]
  (let [dt (/ (delta-t) 1000.0)]
    (-> p
        (update :pos mx/add (mx/scale (:vel p) dt))
        (update :rot + (mx/scale (:rot-vel p) dt)))))


;;;; Shape Component
(defcomponent Shape
  "A component that represents some basic 2D shape that can be drawn.
Usually, the args represent vertices.  If the shape is a circle, there is one arg - the radius."
  [shape-type args edge-color fill-color])

(def render-fn
  "This will contain a no-args function that draws each screen."
  (atom (fn [])))

(defn rotate-vec
  "Rotate a 2d vector by the angle"
  [[x y] angle]
  (let [s (mx/sin angle)
        c (mx/cos angle)]
    [(- (* x c) (* y s))
     (+ (* x s) (* y c))]))

(defsys render-shape
  [e]
  [shp Shape
   phs Phys]
  (reset!
   render-fn
   ;; A lambda for rendering
   (fn render-inner []
     (q/background 100)
     (case (:shape-type shp)
       :circle (let [[rad] (:args shp)
                     [x y] (:pos phs)
                     [x2 y2] (mx/add [x y]
                                     (rotate-vec [rad 0] (:rot phs)))]

                 ;; Make sure our ellipses are centered
                 (q/ellipse-mode :center)

                 ;; Set the fill and stroke colors
                 (apply q/fill (:fill-color shp))
                 (apply q/stroke (:edge-color shp))

                 ;; Draw the ellipse, and a line to represent its direction
                 (q/ellipse x y rad rad)
                 (q/line x y x2 y2))))))

(def current-state
  (atom nil))

(defn setup
  "Setup the main sketch."
  []
  (q/frame-rate 50)
  (q/color-mode :rgb)
  ;; The game state is a control state structure
  (let [ret (ctrl/control-state
             :systems [phys-update
                       render-shape]
             :c-map (game-state [(entity (->Shape :circle [50] [0 0 0] [255 255 255])
                                         (->Phys [100 100]
                                                 [5 0] 0
                                                 Math/PI))])
             :beacon 20)]
    (update-loop ret)
    
    (reset! current-state ret)))

(defn draw-state [state]
  (let [t (q/millis)]
    (when-let [f @render-fn]
      (f))

    (reset! last-frame-millis t))

  (reset! current-state state))

(def current-sketch (atom nil))

(defsys turn-red
  "Turn an entity red."
  [e]
  [shp Shape]
  (assoc shp :fill-color [255 0 0]))

(defsys turn-white
  "Turn an entity white."
  [e]
  [shp Shape]
  (assoc shp :fill-color [255 255 255]))


(defn white-red-loop
  []
  (let [c (chan)]
    (ctrl/controller
     c
     (go-loop []
       (>! c turn-red)
       (<! (timeout 1000))
       (>! c turn-white)
       (<! (timeout 1000))
       (recur)))))

(defn red-white-blinks
  [n]
  (let [c (chan)]
    (ctrl/controller
     c
     (go-loop [i 0]
       (when (< i n)
         (>! c turn-red)
         (<! (timeout 200))
         (>! c turn-white)
         (<! (timeout 1000))
         (recur (inc i)))))))

(defn start-sketch []
  (->>
   (q/sketch
    :title "The Sketch"
    :size [640 480]
    :update identity
    :setup #'setup
    :draw #'draw-state
    :features [:no-start]
    :middleware [qm/fun-mode]
    :on-close (fn [] (async/put! (:control @current-state) (constantly nil))))
   (reset! current-sketch)))
