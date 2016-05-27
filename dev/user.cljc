(ns user
  #_(:require #?(:clj [:clojure.core.async :refer ])))


;; Top level side-effect of setting the global exception handler
;; TODO: Write an async hander that batches up similar exceptions
;; and counts them, notifying the user every one second at most.

#?(:clj
   (Thread/setDefaultUncaughtExceptionHandler
    (reify Thread$UncaughtExceptionHandler
      (uncaughtException [this thread throwable]
        (println (.getMessage throwable))))))
