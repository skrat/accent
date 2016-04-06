(ns accent.signals
  (:require [cljs.core.async :as a]))

(defn |>
  "Creates new channel and pipes in values from `in transduced by `xf"
  [in xf]
  (let [ch (a/chan)]
    (a/pipeline 1 ch xf in)
    ch))

(defn foldp
  "Create a past-dependent signal. Each value given on the input signal will be
  accumulated, producing a new output value."
  [step init]
  (let [prev (volatile! init)]
    (fn [xf]
      (fn
        ([] (xf))
        ([result] (xf result))
        ([result input]
         (let [last (step @prev input)]
           (vreset! prev last)
           (xf result last)))))))

(defn relativize
  "Relativize pointer input (mouse or touch), giving :x and :y data
  relative to last event."
  []
  (let [prev (volatile! nil)]
    (fn [xf]
      (fn
        ([] (xf))
        ([result] (xf result))
        ([result input]
         (let [{:keys [x y type]} input
               [px py] (or @prev [x y])
               skip? (#{"mouseup"} type)]
           (if skip?
             (vreset! prev nil)
             (vreset! prev [x y]))
           (xf result
            (-> input
                (update :x - px)
                (update :y - py)))))))))

(defn tokenize
  "Transform input into [k input]"
  [k]
  (fn [xf]
    (fn
      ([] (xf))
      ([result] (xf result))
      ([result input] (xf result [k input])))))
