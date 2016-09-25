(ns specs.core-test
  (:require [#?(:clj clojure.core.async :cljs cljs.core.async)
             :as async
             :include-macros true
             :refer [chan go <! >! put! take! go-loop
                     timeout alts! alt! close! <!! >!!]]
            [cross-map.core :as cmap
             :refer [cross-map cross cross-cols cross-rows
                     cols rows]]
            [cross-map.util :as u
             :refer [<|]]
            [clojure.test :refer :all]
            [specs.core :refer :all]
            [specs.control :as ctrl]
            [specs.util :as su]

            [clojure.core.matrix :as mx]
            
            [#?(:clj clj-time.core :cljs cljs-time.core)
             :as tm]
            
            ;; 
            (quil [core :as q]
                  [middleware :as qm])))

;;;; A test-map that we can use for cross-referencing!
(def alphabet (map char (range (int \a) (inc (int \z)))))

(def test-rows (range 10))
(def test-cols (map (comp keyword str) (take 10 alphabet)))
(def num-removed 10)

(defn make-test-entry
  [i l]
  (keyword (str (name l) "_" i)))

(let [test-pairs (for [i test-rows
                       l test-cols]
                   [[i l] (make-test-entry i l)])]

  (def test-cmap (into (cross-map) test-pairs))
  (def test-map (into {} test-pairs)))

(deftest profile-test
  ;; Testing wheter profiles match cross-operations

  (let [;; removals (list [2,:c],[2,:d],[2,:e],[4,:f],[6,:b],[7,:c],
        ;;                [7,:f],[3,:j],[4,:j],[5,:j],[6,:j])
        ;; test-cmap (apply dissoc test-cmap removals)
        row-keys [3,4,5,6]
        col-keys [:c,:d,:e]]

    ;;; Matchin profiles
    (is (= ((realize-profile (apply match-eid row-keys)) test-cmap)
           (cross-rows test-cmap row-keys :every-row)))

    (is (= ((realize-profile (apply match-ctype col-keys)) test-cmap)
           (cross-cols test-cmap col-keys :every-col)))
    
    (is (= ((realize-profile (apply for-eid row-keys)) test-cmap)
           (map #(find (rows test-cmap) %) row-keys)))

    (is (= ((realize-profile (apply for-ctype col-keys)) test-cmap)
           (map #(find (cols test-cmap) %) col-keys)))


    ;;; For composite profiles (made with for-entry), there are
    ;;; 4 passing cases that need to be tested, which are combinations of
    ;;; all-rows, any-rows, all-cols, any-cols
    (is (= ((realize-profile (for-entry (apply match-ctype col-keys)
                                        (apply match-eid row-keys)))
            test-cmap)
           (cross test-cmap row-keys col-keys :every-col :every-row)))

    (is (= ((realize-profile (for-entry (apply for-ctype col-keys)
                                        (apply match-eid row-keys)))
            test-cmap)
           (cross test-cmap row-keys col-keys :any-col :every-row)))

    (is (= ((realize-profile (for-entry (apply match-ctype col-keys)
                                        (apply for-eid row-keys)))
            test-cmap)
           (cross test-cmap row-keys col-keys :every-col :any-row)))

    (is (= ((realize-profile (for-entry (apply for-ctype col-keys)
                                        (apply for-eid row-keys)))
            test-cmap)
           (cross test-cmap row-keys col-keys :any-row :any-col)))

    ;;; Incompatible combinations need to throw exceptions
    ;;; TODO: Will this work in JS??
    (is (thrown? AssertionError
                 (realize-profile (for-entry (apply match-ctype col-keys)
                                             (apply for-ctype col-keys)))))

    (is (thrown? AssertionError
                 (realize-profile (for-entry (apply match-eid row-keys)
                                             (apply for-eid col-keys)))))))


;;; For testing defsys
(defsys test-defsys
  [row]
  [c :c
   d :d
   e :e]
  [row c d e])

(defsys test-row-defsys
  [col]
  (bind-eid [three 3
             four 4
             five 5
             six 6])
  [col three four five six])

(deftest sysfn-test
  ;; Tests whether or not sysfns operate as appropriate on
  ;; normal cross-map iteration
  (let [removals (list [2,:c],[2,:d],[2,:e],[4,:f],[6,:b],[7,:c],
                       [7,:f],[3,:j],[4,:j],[5,:j],[6,:j])
        test-cmap (apply dissoc test-cmap removals)
        row-keys [3,4,5,6]
        col-keys [:c,:d,:e]

        ;; This sysfn should select all rows with c, d, and e components,
        ;; and bind the variables appropriately
        test-sysfn (sysfn [row]
                          [c :c
                           d :d
                           e :e]
                          [row c d e])

        prof (profile test-sysfn)
        prof-fn (profile-fn test-sysfn)

        test-sysfn-results (map (<| test-sysfn val) (prof-fn test-cmap))
        test-defsys-results (map (<| test-defsys val) (prof-fn test-cmap))
        compare-results (->> (cross-cols test-cmap [:c :d :e])
                             (map val)
                             (map (fn [row] [row (:c row) (:d row) (:e row)])))

        test-row-sysfn (sysfn [col]
                              (bind-eid [three 3
                                         four 4
                                         five 5
                                         six 6])
                              [col three four five six])

        prof-row (profile test-row-sysfn)
        prof-fn-row (profile-fn test-row-sysfn)
        
        test-row-results (map (<| test-row-sysfn val) (prof-fn-row test-cmap))
        test-row-def-results (map (<| test-row-defsys val) (prof-fn-row test-cmap))

        compare-row-results (->> (cross-rows test-cmap [3 4 5 6])
                                 (map val)
                                 (map (fn [col] [col (col 3) (col 4) (col 5) (col 6)])))]
    (is (= test-sysfn-results test-defsys-results compare-results))
    (is (= test-row-results test-row-def-results compare-row-results))))

(defcomponent Position [x y])
(defcomponent Velocity [dx dy])
(defcomponent Acceleration [ddx ddy])

(defsys update-p
  [e]
  [{:keys [x y]} Position
   {:keys [dx dy]} Velocity]
  (assert (not= *eid* "Still")
          "The Still entity should not be updated by update-v.")
  (->Position (+ x dx) (+ y dy)))

(defsys update-v
  [e]
  [{:keys [dx dy]} Velocity
   {:keys [ddx ddy]} Acceleration]
  (assert (not= *eid* "Still")
          "The Still entity should not be updated by update-v.")
  (assert (not= *eid* "Slow")
          "The Mover entity should not be updated by update-v.")
  (->Velocity (+ dx ddx) (+ dy ddy)))

(deftest run-systems-tst
  ;; Testing whether or not run-systems works
  (let [;; This one has no functions executed on it.
        still-ent (entity-with-id "Still"
                                  (->Position 0 0))
        ;; This one should only have update-p executed on it
        slow-ent (entity-with-id "Slow"
                                  (->Position 0 0)
                                  (->Velocity 1 1))
        ;; This one has both update-v and update-p executed on it
        fast-ent (entity-with-id "Fast"
                                 (->Position 0 0)
                                 (->Velocity 1 1)
                                 (->Acceleration 1 1))

        ;; Update velocity first so that acceleration
        ;; influences position
        system-order [update-v update-p]

        ;; Initial game state
        state (game-state (list still-ent
                                slow-ent
                                fast-ent))

        ;; Subsequent game state
        new-state (run-systems state system-order)

        ;; Destructure the new state's rows to get entities
        {new-still "Still"
         new-slow "Slow"
         new-fast "Fast"} (rows new-state)

        ;;typeses (map (<| type key) new-still)
        ]
    
    (is (isa? (first (keys new-still)) Position))
    (is (= (first (keys new-still)) Position))
    
    (is (= (new-still Position) (->Position 0 0)))
    
    (is (= (new-slow Position) (->Position 1 1)))
    (is (= (new-slow Velocity) (->Velocity 1 1)))

    (is (= (new-fast Position) (->Position 2 2)))
    (is (= (new-fast Velocity) (->Velocity 2 2)))
    (is (= (new-fast Acceleration) (->Acceleration 1 1)))))


;;;; Update loop tests
;;;; For now, this test is interactive rather than automated.

;;;; The test here is a basic game about moving shapes around the screen

(defcomponent Phys
  "The physical attributes of this entity, like position, velocity, angle, etc."
  [pos vel acc
   rot rot-vel rot-acc
   mat mat-vel mat-acc])

(defn rp-mat
  "Given a position and a rotation, make a homogeneous
  matrix that combines both transformations."
  [r [x y :as v]]
  (let [c (mx/cos r)
        s (mx/sin r)]
    [[c (- s) x]
     [s    c  y]
     [0    0  1]]))

(defsys phys-update
  [e]
  [{:keys [pos vel acc
           rot rot-vel rot-acc
           mat mat-vel mat-acc
           trans-mat]
    :as phys} Phys]

  (let [pos (mx/add pos vel)
        vel (mx/add vel acc)
        rot (+ rot rot-vel)
        rot-vel (+ rot-vel rot-acc)
        mat (rp-mat rot pos)
        mat-vel (rp-mat rot-vel vel)
        mat-acc (rp-mat rot-acc acc)]
    
    (-> phys
        (assoc :pos pos :vel vel
               :rot rot :rot-vel rot-vel
               :mat mat :mat-vel mat-vel :mat-acc mat-acc))))



(defcomponent Shape
  "A component that represents some basic 2D shape that can be drawn.
Usually, the args represent vertices.  If the shape is a circle, there is one arg - the radius."
  [shape-type args edge-color fill-color])

(defonce
  render-fns-chan
  (chan (async/sliding-buffer 10)))

(defonce
  render-fns-count
  (atom 0))

;; LEFTOFF: Why is this not executing??
(defsys render-shape
  [e]
  [{:keys [shape-type
           args
           edge-color
           fill-color] :as shp} Shape
   {[x y :as pos] :pos
    :keys [rot] :as phs} Phys]
  ;; We can't directly render in this function, so
  ;; we push a renderfunction to the queue
;;  (println "System's gettin' called, doggo!")
  (put! render-fns-chan
        (fn render-inner []
          #_(println "Inner function is getting called!!")
          (q/fill 255 0 0)
          (q/text (str "Here are the things: " rot ", " args)  0 60)
          (case shape-type 
            :polygon nil
            :circle (let [[rad] args
                          _ (assert (number? rad) "Circle expects single number param.")
                          [[_ lx _] [_ ly _ ] _ :as lmx] (mx/mul (rp-mat rot [0 0]) [0 rad 1])]
                      (q/ellipse-mode :center)
                      (apply q/fill fill-color)
                      (apply q/stroke edge-color)
                      ;; TODO: is it rigt to use the rad directly like this?
                      
                      (q/ellipse x y rad rad)
                      ;;(q/text (str "lmx: " lmx) 0 80)
                      (q/line x y lx ly)
                      nil)
            ;; Default
            nil)))
  (swap! render-fns-count inc)
  e)

(defn setup
  "Setup for the main sketch."
  []
  (q/frame-rate 50)
  (q/color-mode :rgb)
  ;; The game state is a control state structure
  (let [ret (ctrl/control-state
             :systems [phys-update
                       render-shape]
             :c-map (game-state (list (entity (->Shape :circle [50] [0 0 0]
                                                       [255 255 255])
                                              (->Phys [100 100 1]
                                                      [0 0 1]
                                                      [0 0 1]
                                                      0
                                                      0
                                                      0
                                                      [[1 0 0]
                                                       [0 1 0]
                                                       [0 0 1]]
                                                      [[1 0 0]
                                                       [0 1 0]
                                                       [0 0 1]]
                                                      [[1 0 0]
                                                       [0 1 0]
                                                       [0 0 1]]))))
             :beacon 20)]
    (update-loop ret)
    ret))

(def framecounter (atom 0))

(defn draw-state [{:keys [beacon
                          beacon-notify]
                   :as state}]
  (q/background 0)

  (q/fill 255)

  (q/ellipse 200 200 50 50)

  (q/fill 0 255 0)
  (q/text (str "Frame-counter: " (swap! framecounter inc)) 0 20)
  (q/text (str "Fn-counter: " @render-fns-count) 0 40)

  (when-let [f (async/poll! render-fns-chan)]
    (swap! render-fns-count dec)
    (f)
    )
  
  #_(try (do
         (q/text "Putting beacon..."0 40)
         (put! beacon true)
         (let [ns (<!! beacon-notify)]
           (q/fill 255)
           (q/text (str "State: " ns) 0 60)))
       (catch Exception e
         (q/fill 255 0 0)
         (q/text (str "Error: " e 0 60))))
  
  state)


(def current-sketch
  (atom nil))

(defn start-sketch []
  (->>
   (q/sketch
    :title "The Sketch"
    :size [640 480]
    :update identity
    :setup #'setup
    :draw #'draw-state
    :features [:no-start]
    :middleware [qm/fun-mode])
   (reset! current-sketch)))


(def vec-state (atom nil))

(def init-vectors
  [[1 1] [-1 1]])

(defn v-setup
  []
  (let [ratio (/ (min (q/width) (q/height)) 2.0)]
    (q/frame-rate 60)
    (q/color-mode :rgb)
    (q/smooth)
    
    (q/stroke-weight 0.01)
    (->> {:ratio ratio
          :center [(/ (q/width) ratio 2.0)
                   (/ (q/height) ratio 2.0)]
          :vecs (map (fn [v]
                       (vary-meta v update :color
                                  #(or % [(rand-int 255)
                                          (rand-int 255)
                                          (rand-int 255)])))
                     init-vectors)}
         (reset! vec-state))))

(def printed-matrix
  (atom "NO_MAT"))

(defn v-draw
  [{[w h] :center
    :keys [ratio ] :as state}]
  (q/background 0)
  (q/push-matrix)

  ;; Scale 
  (q/scale ratio, (- ratio))
  (q/translate w (-  h))
  
  (q/stroke 255)
  (q/line [0 0] [1 1])
  (q/rect 0 0 1 1)
  
  (reset! printed-matrix
          (with-out-str
            (q/print-matrix)))
  (q/pop-matrix)
  )

(defn vector-sketch
  []
  (q/sketch
   :title "Vectors"
   :size [640 480]
   :update identity
   :draw #'v-draw
   :setup #'v-setup
   :middleware [qm/fun-mode]))
