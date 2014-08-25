;; Based on Elm's Signal module
;; http://library.elm-lang.org/catalog/elm-lang-Elm/0.12.3/Signal

(ns accent.signals
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :refer [chan put! <! alts!]]))

(defn transform
"Create transformed signal whose values are transformed with f."
  [f in]
  (let [output (chan)]
    (go (loop []
      (put! output (f (<! in)))
      (recur)))
    output))

(defn foldp
"Create a past-dependent signal. Each value given on the input signal will be
 accumulated, producing a new output value.

 For instance, (foldp (fps 40) + 0) is the time the program has been running,
 updated 40 times a second."
  [ch step init]
  (let [output (chan)]
    (go (loop [past init]
      (let [current (step past (<! ch))]
        (put! output current)
        (recur current))))
    output))

(defn sample-on
"Sample from the second input every time an event occurs on the first input.
 For example, (sample-on clicks (every second)) will give the approximate time
 of the latest click."
  ([source clock]
   (sample-on clock source nil))
  ([source clock state]
   (let [output (chan)]
     (go (loop [v state]
       (let [[v' ch] (alts! [clock source])
              v'' (or (when (= ch source) v') v)]
         (when (and v'' (= ch clock))
           (put! output v''))
         (recur v''))))
     output)))
