
(ns specs.util
  "Utility functions for specs.")

(defmacro let>
  "A version of let that can be embedded
  into a -> expression.  The binding vector
  accepts only an odd number of forms, where
  the first binding has v as its value."
  [v [b & bindings] & args]
  `(let ~(apply vector b v bindings)
     ~@args))

(defmacro let>>
  "A version of let that can be embedded
  into a ->> expression.  The binding vector
  accepts only an odd number of forms, where
  the first binding has the last element of args
  as its value."
  [[b & bindings] & args]
  (let [v (last args)
        args (drop-last args)]
    `(let ~(apply vector b v bindings)
       ~@args)))

(defn queue
  "Create a persistent queue."
  [& args]
  #?(:clj  (into clojure.lang.PersistentQueue/EMPTY args)
     :cljs (into #queue[] args)))

(defn queue? [q]
  #?(:clj  (instance? clojure.lang.PersistentQueue q)
     :cljs (instance? cljs.core.PersistentQueue q)))
