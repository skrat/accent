(ns accent.drawables
  (:require [accent.arrays :as ta]
            [accent.buffers :as buffers]
            [accent.constants :as GL]
            [accent.context :refer [gl]]
            [accent.shaders :as shaders]))

(defrecord Pointer [attr
                    size
                    offset
                    stride])

(defrecord Drawable [start
                     size
                     mode
                     buffer
                     data
                     pointers
                     uniforms])

(def float-size (.-BYTES_PER_ELEMENT js/Float32Array))

(defn set-pointer!
  [program pointer]
  (let [{:keys [attr size offset stride]} pointer
        loc (shaders/get-attribute-location program (name attr))]
    (when (>= loc 0)
      (.vertexAttribPointer gl
                            loc
                            size
                            GL/float
                            false
                            (* stride float-size)
                            (* offset float-size)))))

(defn set-pointers-for-shader!
  [program drawable]
  (buffers/bind! GL/array-buffer (:buffer drawable))
  (doseq [p (:pointers drawable)]
    (set-pointer! program p)))

(defn create!
  ([data size pointers]
   (create! data size pointers nil))
  ([data size pointers uniforms]
   (let [array    (if (ta/typed-array? data) data (ta/float32 data))
         buffer   (buffers/create! array GL/array-buffer GL/static-draw)
         ptrs     (for [[attr args] pointers]
                    (apply ->Pointer (into [attr] args)))]
     (Drawable. 0 size GL/triangles buffer data ptrs uniforms))))

(defn draw!
  [{:keys [mode start size]}]
  (.drawArrays gl mode start size))

;; Common drawables
;; ================

(defn quad []
  (create! (ta/float32 [-1 -1   1 -1   1 1
                        -1 -1   1  1  -1 1])
           6 {:position [2 0 2]}))

;; Transformations
;; ===============

(defn barycentric!
  [drawable]
  (let [{:keys [size pointers data]} drawable
        stride (apply max (map #(:stride %) pointers))
        data-bary (flatten
                    (for [tri (partition 3
                               (partition stride (ta/->clj data)))]
                      [(concat (nth tri 0) [1 0 0])
                       (concat (nth tri 1) [0 1 0])
                       (concat (nth tri 2) [0 0 1])]))
        array    (ta/float32 data-bary)
        buffer-bary (buffers/create! array GL/array-buffer GL/static-draw)
        offset-bary (apply max (map #(+ (:offset %) (:size %)) pointers))
        stride-bary (+ 3 stride)
        pointers-bary (conj (map #(assoc % :stride stride-bary) pointers)
                            (Pointer. :barycentric 3 offset-bary stride-bary))]
    (merge drawable {:pointers pointers-bary
                     :buffer buffer-bary
                     :data data-bary})))
