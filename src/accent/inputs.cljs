(ns accent.inputs
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :refer [chan put! <!]]
            [accent.signals :as s]
            [goog.events :as g]))

(def default-element js/document.body)

(defn listen
  [el types mapping]
  (let [output (chan)
        handler
        (fn [^g/BrowserEvent evt]
          (.preventDefault evt)
          (put! output (mapping evt)))]
    (doseq [t types]
      (g/listen el (name t) handler))
    output))

(defn generic-event->map
  [^g/BrowserEvent evt]
  {:type    (.-type       evt)
   :alt     (.-altKey     evt)
   :ctrl    (.-ctrlKey    evt)
   :meta    (.-metaKey    evt)
   :shift   (.-shiftKey   evt)})

(defn mouse-event->map
  [^g/BrowserEvent evt]
  (->
   (generic-event->map evt)
   (merge {:x      (.-clientX   evt)
           :y      (.-clientY   evt)
           :button (.-button    evt)})))

(def mouse-event-types
  [:click
   :dblclick
   :mousedown
   :mouseup
   :mousemove])

(defn mouse
"Create new (async.core) channel for mouse events (click, dblclick, mousedown,
 mouseup, mousemove). Values are maps with keys
 :x :y :button :type :alt :ctrl :meta :shift."
  ([types]
   (mouse types default-element))
  ([types el]
   (listen el (or types mouse-event-types) mouse-event->map)))

(defn keyboard-event->map
  [^g/BrowserEvent evt]
  (->
   (generic-event->map evt)
   (merge {:code (.-keyCode evt)
           :char (js/String.fromCharCode (.-keyCode evt))})))

(def keyboard-event-types
  [:keypress
   :keydown
   :keyup])

(defn keyboard
"Create new (async.core) channel for keyboard events (keypress, keydown, keyup).
 Value are maps with keys - :code :char :type :alt :ctrl :meta :shift."
  ([types]
   (keyboard types default-element))
  ([types el]
   (listen el (or types keyboard-event-types) keyboard-event->map)))

(filter (comp #{1} :butt) [{:butt 1 :foo 2} {:butt 0 :foo 3}])

(defn drag
  "Dragging occurs in between `mousedown` and `mouseup` events."
  ([el]
   (let [output (chan)
         mice   (mouse nil el)]
     (go
       (loop [down false]
         (let [data (<! mice)]
           (when down
             (put! output data))
           (case (:type data)
             "mousedown" (recur true)
             "mouseup"   (recur false)
                         (recur down)))))
     output)))

(defn wheel-event->map
  [^g/BrowserEvent evt]
  (let [evt' (.-event_ evt)]
    (->
     (generic-event->map evt)
     (assoc :delta (if-let [delta (.-wheelDelta evt')]
                     (/ delta 120)
                     (- (.-detail evt')))))))

(defn wheel
  "Mouse wheel events. Returns a channel with values -1 or 1 depending on
  the wheel movement direction."
  ([] (wheel default-element))
  ([el]
   (listen el [:mousewheel :DOMMouseScroll] wheel-event->map)))
