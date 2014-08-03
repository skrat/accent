(ns accent.schedule
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :refer [chan put! alts! sliding-buffer]]))

(defn asap
"Creates a (core.async) channel to be used for drawing frames
 as soon as possible, that is, it uses `requestAnimationFrame` directly.
 Optionally pass a control channel, and put :stop onto it to close."
  ([] (asap (chan)))
  ([ctrl]
   (let [output (chan (sliding-buffer 1))
         target-loop
           (fn [frame continue]
             (go
               (let [stop? (= :stop (first (alts! [ctrl] :default nil)))]
                 (when-not stop?
                   (.requestAnimationFrame js/window
                     (fn [_] (continue (inc frame) continue))))
                 (put! output frame))))]
     (target-loop 1 target-loop)
      output)))

(defn fixed
"Creates a (core.async) channel to be used for drawing frames
 using `requestAnimationFrame` with rate limiting using fps (FPS) arg.
 Optionally pass a control channel, and put :stop onto it to close."
  ([fps] (fixed fps (chan)))
  ([fps ctrl]
   (let [output (chan (sliding-buffer 1))
         interval (/ 1000 fps)
         target-loop
           (fn [frame then continue]
             (go
               (let [now (js/Date.now)
                     delta (- now then)
                     advance? (> delta interval)
                     next-then (- now (rem delta interval))
                     next-frame (if advance? (inc frame) frame)
                     stop? (= :stop (first (alts! [ctrl] :default nil)))]
                 (when-not stop?
                   (.requestAnimationFrame js/window
                     (fn [_] (continue next-frame next-then continue))))
                 (when advance?
                   (put! output frame)))))]
     (target-loop 1 (js/Date.now) target-loop)
      output)))
