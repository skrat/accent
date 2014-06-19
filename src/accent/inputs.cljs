(ns accent.inputs
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :refer [chan put! <! alts!]]
            [goog.events :as gevents]))

(defn listen
  [el types token mapping]
  (let [output (chan)
        handler (fn [evt]
                  (.preventDefault evt)
                  (put! output [token (mapping evt)]))]
    (doseq [t types]
      (gevents/listen el t handler))
    output))

(defn generic-event->map [evt]
  {:type    (.-type       evt)
   :alt     (.-altKey     evt)
   :ctrl    (.-ctrlKey    evt)
   :meta    (.-metaKey    evt)
   :shift   (.-shiftKey   evt)})

(defn mouse-event->map [evt]
  (merge (generic-event->map evt)
  {:x      (.-clientX   evt)
   :y      (.-clientY   evt)
   :button (.-button    evt)}))

(def mouse-event-types
  ["click"
   "dblclick"
   "mousedown"
   "mouseup"
   "mousemove"])

(defn mouse
"Create new (async.core) channel for mouse events (click, dblclick, mousedown,
 mouseup, mousemove). Events can be filtered using `core.async/filter<`.
 Values are maps with keys - :x :y :button :type :alt :ctrl :meta :shift."
  ([token]
   (mouse token js/document.body))
  ([token el]
   (mouse token el mouse-event-types))
  ([token el types]
   (listen el types token mouse-event->map)))

(defn keyboard-event->map [evt]
  (merge (generic-event->map evt)
  {:code (.-keyCode evt)
   :char (js/String.fromCharCode (.-keyCode evt))}))

(def keyboard-event-types
  ["keypress"
   "keydown"
   "keyup"])

(defn keyboard
"Create new (async.core) channel for keyboard events (keypress, keydown, keyup).
 Events can be filtered using `core.async/filter<`. Value are maps with
 keys - :code :char :type :alt :ctrl :meta :shift."
  ([token]
   (keyboard token js/document.body))
  ([token el]
   (keyboard token el keyboard-event-types))
  ([token el types]
   (listen el types token keyboard-event->map)))

(defn drag
"Dragging occurs in between `mousedown` and `mouseup` events."
  ([token]
   (drag token js/document.body))
  ([token el]
   (let [output (chan)
         mice (mouse token)]
     (go
      (loop [down false]
        (let [[_ data] (<! mice)]
          (when down
            (put! output [token data]))
          (case (:type data)
            "mousedown" (recur true)
            "mouseup" (recur false)
            (recur down)))))
     output)))
