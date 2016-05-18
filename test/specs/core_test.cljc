(ns specs.core-test
  (:require [cross-map.core :as cmap
             :refer [cross-map cross cross-cols cross-rows
                     cols rows]]
            [cross-map.util :as u
             :refer [<|]]
            [clojure.test :refer :all]
            [specs.core :refer :all]))

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

