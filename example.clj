(ns example
  (:use [clojure.string :only [split-lines]]))

(set! *warn-on-reflection* true)

(definterface Counter
  (get [])
  (increment [n])
  (log []))

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

(defn ^Counter make-Counter [file]
  (reify Counter
    (get [this]
      (reduce + (.log this)))
    (log [this]
      (mapv read-string (split-lines (slurp file))))
    (increment [this n]
      (spit file (prn-str n) :append true))))

(defn doit [^Counter c times]
  (dotimes [i times]
    (.increment c i))
  (.get c))
