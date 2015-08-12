(ns accent.drawables
  (:require [accent.arrays :as ta]
            [accent.buffers :as buffers]
            [accent.symbols :as GL]
            [accent.context :refer [*gl*]]
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
      (.vertexAttribPointer *gl*
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
   (create! data size pointers uniforms GL/triangles))
  ([data size pointers uniforms mode]
   (let [array    (if (ta/typed-array? data) data (ta/float32 data))
         buffer   (buffers/create! array GL/array-buffer GL/static-draw)
         ptrs     (for [[attr args] pointers]
                    (apply ->Pointer (into [attr] args)))]
     (Drawable. 0 size mode buffer array ptrs uniforms))))

(defn update!
  [{:keys [buffer data] :as drawable} new-data]
  (.set data (clj->js new-data))
  (buffers/bind! GL/array-buffer buffer)
  (buffers/data! GL/array-buffer data GL/static-draw)
  drawable)

(defn update-uniforms [drawable f & args]
  (update-in drawable [:uniforms]
    #(for [[n u] %] [n (apply f (cons u args))])))

(defn draw!
  [program {:keys [mode start size uniforms]}]
  (if uniforms
    (loop [us uniforms
           offset start]
      (let [[n vars] (first us)
             r (rest us)]
        (doseq [[k [dtype value]] vars]
          (shaders/set-uniform! program k dtype value))
        (.drawArrays *gl* mode offset n)
        (when-not (empty? r)
          (recur r (+ offset n)))))
    (.drawArrays *gl* mode start size)))

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
        bary-offset (apply max (map #(+ (:offset %) (:size %)) pointers))
        bary-stride (+ 3 stride)
        bary-data   (let [ary (ta/float32 (* bary-stride (/ (count data) stride)))]
                      (doseq [i (range 0 (/ (count data) stride))]
                        (.set ary
                          (.subarray data
                            (* i stride) (+ stride (* i stride)))
                          (* i bary-stride))
                        (case (rem i 3)
                          0 (.set ary #js [1 0 0] (+ stride (* i bary-stride)))
                          1 (.set ary #js [0 1 0] (+ stride (* i bary-stride)))
                          2 (.set ary #js [0 0 1] (+ stride (* i bary-stride)))))
                      ary)
        bary-buffer (buffers/create! bary-data GL/array-buffer GL/static-draw)
        bary-ptrs   (conj (map #(assoc % :stride bary-stride) pointers)
                            (Pointer. :barycentric 3 bary-offset bary-stride))]
    (merge drawable {:pointers bary-ptrs
                     :buffer bary-buffer
                     :data bary-data})))
