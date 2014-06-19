(ns accent.drawables
  (:require [accent.arrays :as ta]
            [accent.buffers :as buffers]
            [accent.constants :as GL]
            [accent.context :refer [gl]]
            [accent.shaders :as shaders]))

(defrecord Pointer [name size offset stride])

(defrecord Drawable [start size mode buffer pointers data])

(def float-size (.-BYTES_PER_ELEMENT js/Float32Array))

(defn set-pointer!
  [program pointer]
  (let [{:keys [name size offset stride]} pointer
        loc (shaders/get-attribute-location program name)]
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
  [data size pointers-args]
  (let [array    (if (ta/typed-array? data) data (ta/float32 data))
        buffer   (buffers/create! array GL/array-buffer GL/static-draw)
        pointers (for [args pointers-args]
                   (apply ->Pointer
                          (assoc args 0 (name (first args)))))]
    (Drawable. 0 size GL/triangles buffer pointers data)))

(defn draw!
  [{:keys [mode start size]}]
  (.drawArrays gl mode start size))

;; Common drawables
;; ================

(defn quad []
  (create! (ta/float32 [-1 -1   1 -1   1 1
                        -1 -1   1  1  -1 1])
           6 [[:position 2 0 2]]))

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
        buffer   (buffers/create! array GL/array-buffer GL/static-draw)
        offset-bary (apply max (map #(+ (:offset %) (:size %)) pointers))
        stride-bary (+ 3 stride)
        pointers-bary (conj (map #(assoc % :stride stride-bary) pointers)
                            (Pointer. "barycentric" 3 offset-bary stride-bary))]
    (Drawable. 0 size GL/triangles buffer pointers-bary data-bary)))
