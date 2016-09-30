(ns example
  (:use [clojure.string :only [split-lines]]))

;; definterface doesn't generate the corresponding top-level functions
;; like defprotocol does. You need to call (.log foo) instead of (log
;; foo). This is nice because method calls *used to* be faster than
;; function calls.
;;
;; *warn-on-reflection* gives a warning if clojure
;; can't make the efficient direct method call but needs to use
;; reflection to figure out which class is being called.
(set! *warn-on-reflection* true)

(definterface Counter
  (get [])
  (increment [n])
  (log []))

;; Note: in contrast to deftype, val-r and log-r are completely
;; hidden (private). Also, we only need one top-level definition (this
;; function) instead of two (the deftype and the constructor function).
;;
;; The return type is type hinted. This acts both as documentation and makes e.g.
;; (.log (fake-Counter)) not give a reflection warning.
(defn ^Counter fake-Counter []
  (let [val-r (ref 0)
        log-r (ref [])]
    (reify Counter
      (get [this]
        @val-r)
      (log [this]
        @log-r)
      (increment [this n]
        (dosync
         (alter val-r + n)
         (alter log-r conj n))))))

(defn ^Counter make-Counter [& [file]]
  (let [file (or file "/tmp/counter")]
    (reify Counter
      (get [this]
        (reduce + (.log this)))
      (log [this]
        (mapv read-string (split-lines (slurp file))))
      (increment [this n]
        (spit file (prn-str n) :append true)))))

;; Note method calls instead of function calls. Type hint is again
;; both documentation and inhibits reflection warning.
(defn doit [^Counter c times]
  (dotimes [i times]
    (.increment c i))
  (.get c))
