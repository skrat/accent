(ns accent.nodes
  (:require [accent.buffers :as buffers]
            [accent.const :as GL]
            [accent.context :refer [gl]]
            [accent.drawables :as drawables]
            [accent.shaders :as shaders]
            [accent.textures :as textures]))

(defrecord Node [drawables
                 program
                 props
                 state
                 frame
                 color
                 depth])

(def default-props {:front        false
                    :depth-buffer false
                    :channels     GL/rgba
                    :format       GL/rgba
                    :type         GL/unsigned-byte
                    :filter       GL/nearest
                    :viewport     [0 0 640 480]})

(def default-state {:blend        nil
                    :cull-face    nil
                    :depth-test   false
                    :depth-write  false
                    :front-face   nil})

(def current-state (atom {}))

(defn diff-state-key
  [diff [k v]]
  (if (not= v (k @current-state))
    (assoc diff k v)
    diff))

(defn diff-state
  [state]
  (reduce diff-state-key {} state))

(defn enable-disable!
  [enum switch]
  (if switch
    (.enable  gl enum)
    (.disable gl enum)))

(defn set-state!
  [state]
  (doseq [[k v] (diff-state state)]
    (case k
      :blend
        (do
          (enable-disable! GL/blend v)
          (when v
            (let [[mode sfactor dfactor] v]
              (.blendEquation gl mode)
              (.blendFunc gl sfactor dfactor))))
      :cull-face
        (do
          (enable-disable! GL/cull-face v)
          (when v
            (.cullFace gl v)))
      :depth-test
        (do
          (enable-disable! GL/depth-test v)
          (when v
            (.depthFunc gl v)))
      :depth-write
        (.depthMask gl v)
      :front-face
        (if v
          (.frontFace gl v)
          (.frontFace gl GL/ccw))))
  (reset! current-state state))

(defn revert-state! []
  (set-state! default-state))

(defn set-viewport!
  [[xoff yoff width height]]
  (.viewport gl xoff yoff width height))

(defn create-buffer! [{:keys [filter
                              depth-buffer
                              viewport
                              channels
                              format
                              type]}]

  (let [[width height] (drop 2 viewport)
        color (textures/create-2D! channels format type 0)
        depth (when depth-buffer (buffers/create-render!))
        frame (buffers/create-frame!)]
    (textures/bind! color)
    (textures/set-filter! color filter)
    (textures/set-wrap! color GL/clamp-to-edge)
    (textures/set-size! color width height)
    (textures/unbind! color)
    (buffers/bind-frame! frame)
    (buffers/attach-color! color)
    (when depth-buffer
      (buffers/bind-render! depth)
      (buffers/set-storage! GL/depth-component16 width height)
      (buffers/unbind-render!)
      (buffers/attach-depth! depth))
    (buffers/check-frame frame)
    (buffers/unbind-frame!)

    {:frame frame
     :color color
     :depth depth}))

(defn create-node!
  ([drawables program]
   (create-node! drawables
                 program {} {}))
  ([drawables program props]
   (create-node! drawables
                 program
                 props {}))
  ([drawables program props state]
   (let [full-props (merge default-props props)
         full-state (merge default-state state)]
     (merge (Node. drawables program full-props full-state nil nil nil)
            (create-buffer! full-props)))))

(defn begin!
  [node]
  (set-viewport! (-> node :props :viewport))
  (set-state! (:state node))
  (shaders/use! (:program node))
  (when-not (-> node :props :front)
    (buffers/bind-frame! (:frame node))))

(defn draw!
  [{:keys [program drawables] :as node} uniforms]
  (shaders/set-uniforms! program
    (assoc uniforms :viewport
      [:val2 (drop 2 (-> node :props :viewport))]))
  (doseq [d drawables]
    (drawables/set-pointers-for-shader! program d)
    (drawables/draw! program d)))

(defn end!
  [node]
  (when-not (-> node :props :front)
    (buffers/unbind-frame!)))
