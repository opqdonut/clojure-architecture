(ns example)

(set! *warn-on-reflection* true)

(definterface Foo
  (frobnicate [])
  (mogrify [x y]))

(defn ^Foo fake-Foo []
  (let [state-a (atom {:frobnicate 1 :data []})]
    (reify Foo
      (frobnicate [this]
        (swap! state-a update-in [:frobnicate] inc))
      (mogrify [this x y]
        (swap! state-a update-in [:data] conj {:x x :y y})
        (.frobnicate this)
        :ok))))

(defn ^Foo make-Foo [host port]
  nil)

(defn doit [^Foo foo times]
  (dotimes [i times]
    (.frobnicate foo)
    (.mogrify foo i true)))
