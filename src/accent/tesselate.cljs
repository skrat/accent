(ns accent.tesselate)

(def primitive-type (.-primitiveType js/libtess))
(def GL-TRIANGLES (.-GL_TRIANGLES primitive-type))

(def glu-enum (.-gluEnum js/libtess))
(def GLU-TESS-VERTEX-DATA (.-GLU_TESS_VERTEX_DATA glu-enum))
(def GLU-TESS-BEGIN (.-GLU_TESS_BEGIN glu-enum))
(def GLU-TESS-ERROR (.-GLU_TESS_ERROR glu-enum))
(def GLU-TESS-COMBINE (.-GLU_TESS_COMBINE glu-enum))
(def GLU-TESS-EDGE-FLAG (.-GLU_TESS_EDGE_FLAG glu-enum))

(defn weight [ws xs]
  (for [[v w] (map vector xs ws)]
    (* v w)))

(defn weight-many [ws vs]
  (for [transposed (apply map vector vs)]
    (map #(apply + (weight ws %))
          (apply map vector transposed))))

(defn begin-callback [type]
  (when (not= type GL-TRIANGLES)
    (js/console.error (str "Expected GL_TRIANGLES but got " type))))

(defn vertex-callback
  [data verts]
  (.push verts data))

(defn error-callback [errno]
  (js/console.error (str "Error no.: " errno)))

(defn combine-callback
  [coords data weight]
  (if (= nil (nth (aget data 0) 1))
    [coords nil]
    [coords (weight-many
              (remove nil? (js->clj weight))
              (for [d (remove nil? (js->clj data))]
                (nth d 1)))]))

(defn edge-callback [flag])

(def tessy
  (doto (js/libtess.GluTesselator.)
    (.gluTessCallback GLU-TESS-BEGIN begin-callback)
    (.gluTessCallback GLU-TESS-VERTEX-DATA vertex-callback)
    (.gluTessCallback GLU-TESS-ERROR error-callback)
    (.gluTessCallback GLU-TESS-COMBINE combine-callback)
    (.gluTessCallback GLU-TESS-EDGE-FLAG edge-callback)))

(defn normal [poly]
  (let [[a b c] (map clj->js (take 3 poly))]
    (js/vec3.normalize #js []
        (js/vec3.cross #js [] (js/vec3.sub #js [] b a)
                              (js/vec3.sub #js [] c a)))))

(defn tesselate
  ([poly]
   (tesselate poly (repeat (count poly) [])))
  ([poly data]
   (let [[nx ny nz] (js->clj (normal poly))
         verts #js []]
     (doto tessy
       (.gluTessNormal nx ny nz)
       (.gluTessBeginPolygon verts)
       (.gluTessBeginContour))
     (doseq [[v d] (map vector poly data)]
       (.gluTessVertex tessy (clj->js v) [v d]))
     (doto tessy
       (.gluTessEndContour)
       (.gluTessEndPolygon))
     (js->clj verts))))
