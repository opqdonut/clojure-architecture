(ns counterpoint
  (:use [clojure.string :only [split-lines]]))

(defprotocol counter
  (get [this])
  (increment [this n])
  (log [this]))

(deftype fake-counter [val-r log-r]
  counter
  (get [this]
    @val-r)
  (log [this]
    @log-r)
  (increment [this n]
    (dosync
     (alter val-r + n)
     (alter log-r conj n))))

(defn new-fake-counter []
  (->fake-counter (ref 0) (ref [])))

(deftype real-counter [file]
  counter
  (get [this]
    (reduce + (log this)))
  (log [this]
    (mapv read-string (split-lines (slurp file))))
  (increment [this n]
    (spit file (prn-str n) :append true)))

(defn new-real-counter [& [file]]
  (let [file (or file "/tmp/counter")]
    (->real-counter file)))

(defn doit [c times]
  (dotimes [i times]
    (increment c i))
  (get c))
