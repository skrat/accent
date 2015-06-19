(ns accent.textures
  (:require [accent.symbols :as GL]
            [accent.context :refer [gl]]))

(defrecord Texture [handle target unit
                    channels
                    format
                    type])

(defn bind!
  ([texture]
   (bind! texture nil))
  ([{:keys [unit target handle]} unit-override]
   (.activeTexture gl (+ GL/texture0 (or unit-override unit)))
   (.bindTexture gl target handle)))

(defn unbind!
  ([texture]
   (unbind! texture nil))
  ([{:keys [unit target]} unit-override]
   (.activeTexture gl (+ GL/texture0 (or unit-override unit)))
   (.bindTexture gl target nil)))

(defn set-filter!
  ([texture filter]
   (set-filter! texture filter filter))
  ([{:keys [target]} min-filter mag-filter]
   (.texParameteri gl target GL/texture-min-filter min-filter)
   (.texParameteri gl target GL/texture-mag-filter mag-filter)))

(defn set-wrap!
  ([texture wrap]
   (set-wrap! texture wrap wrap))
  ([{:keys [target]} wrap-s wrap-t]
   (.texParameteri gl target GL/texture-wrap-s wrap-s)
   (.texParameteri gl target GL/texture-wrap-t wrap-t)))

(defn set-size!
  [{:keys [target channels format type]} width height]
  (.texImage2D gl target 0 channels width height 0 format type nil))

(defn upload-data!
  [{:keys [target channels format type]} data width height]
  (.texImage2D gl target 0 channels width height 0 format type data))

(defn upload!
  [{:keys [target channels format type]} image]
  (.texImage2D gl target 0 channels format type image))

(defn create-2D!
  ([]
   (create-2D! GL/rgba
               GL/rgba
               GL/unsigned-byte
               0))
  ([channels format type unit]
   (Texture. (.createTexture gl) GL/texture-2d unit
               channels format type)))

(defn destroy!
  [{:keys [handle]}]
  (.deleteTexture gl handle))
