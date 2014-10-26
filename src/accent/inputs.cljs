(ns accent.inputs
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :refer [chan put! <! filter<]]
            [goog.events :as gevents]))

(def default-element js/document.body)

(defn listen
  [el types token mapping]
  (let [output (chan)
        handler (fn [evt]
                  (.preventDefault evt)
                  (put! output [token (mapping evt)]))]
    (doseq [t types]
      (gevents/listen el (name t) handler))
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
  [:click
   :dblclick
   :mousedown
   :mouseup
   :mousemove])

(defn mouse
"Create new (async.core) channel for mouse events (click, dblclick, mousedown,
 mouseup, mousemove). Events can be filtered using `core.async/filter<`.
 Values are maps with keys - :x :y :button :type :alt :ctrl :meta :shift."
  ([token]
   (mouse token mouse-event-types))
  ([token types]
   (mouse token types default-element))
  ([token types el]
   (listen el (or types mouse-event-types) token mouse-event->map)))

(defn keyboard-event->map [evt]
  (merge (generic-event->map evt)
  {:code (.-keyCode evt)
   :char (js/String.fromCharCode (.-keyCode evt))}))

(def keyboard-event-types
  [:keypress
   :keydown
   :keyup])

(defn keyboard
"Create new (async.core) channel for keyboard events (keypress, keydown, keyup).
 Events can be filtered using `core.async/filter<`. Value are maps with
 keys - :code :char :type :alt :ctrl :meta :shift."
  ([token]
   (keyboard token keyboard-event-types))
  ([token types]
   (keyboard token types default-element))
  ([token types el]
   (listen el (or types keyboard-event-types) token keyboard-event->map)))

(defn drag
"Dragging occurs in between `mousedown` and `mouseup` events."
  ([token]
   (drag token default-element))
  ([token el]
   (let [output (chan)
         mice (mouse token nil el)]
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

(defn relative
"Relativize pointer input (mouse or touch), giving :x and :y data
 relative to last event."
  [ch]
  (let [output (chan)]
    (go
     (loop [mem nil]
       (let [[token data] (<! ch)
             {:keys [x y type]}  data
             [mx my] (or mem [x y])
             rx (- x mx)
             ry (- y my)
             skip? (#{"mouseup"} type)]
         (put! output [token (merge data {:x rx :y ry})])
         (recur (if skip? nil [x y])))))
    output))

(defn wheel-event->map [evt]
  (let [evt' (.-event_ evt)]
    (merge (generic-event->map evt)
      {:delta (if-let [delta (.-wheelDelta evt')]
                (/ delta 120)
                (- (.-detail evt')))})))

(defn wheel
"Mouse wheel events. Returns a channel with values -1 or 1 depending on
 the wheel movement direction."
  ([token]
   (wheel token default-element))
  ([token el]
   (listen el [:mousewheel :DOMMouseScroll]
           token wheel-event->map)))
