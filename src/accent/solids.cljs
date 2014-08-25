;; Module for converting CSG (csg.js) objects to drawables
;; see https://github.com/evanw/csg.js/

(ns accent.solids
  (:require [accent.arrays :as ta]
            [accent.drawables :as drawables]
            [accent.tesselate :refer [tesselate]]))

(defn vec->clj [v]
  [(.-x v) (.-y v) (.-z v)])

(defn poly->clj [poly]
  (let [    shared (.-shared poly)
          vertices (.-vertices poly)
         positions (map vec->clj (map #(.-pos %) vertices))
           normals (map vec->clj (map #(.-normal %) vertices))]
    {:shared shared
     :vertices (map vector positions (map vector normals))}))

(defn quad->tris
  [[a b c d]]
  [[a b c] [a c d]])

(defn poly->tris [poly]
  (let [verts (map first poly)
         data (map second poly)
         tris (tesselate verts data)]
    (partition 3 tris)))

(defn triangulate [faces]
  (let [{tris 3 quads 4} (group-by count faces)
        polygons (filter #(> (count %) 4) faces)]
    (concat tris
            (apply concat (map quad->tris quads))
            (apply concat (map poly->tris polygons)))))

(defn csg->drawable
"Converts CSG object to drawable. Shared property will be available to shaders
 as uniform int, and thus is expected to be int in the CSG object."
  ([csg]
   (csg->drawable csg nil))
  ([csg drawable]
   (let [     faces (group-by #(:shared %) (map poly->clj (.-polygons csg)))
          by-shared (for [[shared fs] faces]
                      [shared (apply concat (triangulate (map :vertices fs)))])
          triangles (apply concat (map second by-shared))
          positions (map first triangles)
            normals (map #(first (second %)) triangles)
           vertices (apply concat (interleave positions normals))]
     (if drawable
       (drawables/update! drawable vertices)
       (drawables/create! (ta/float32 vertices) (count positions)
                          {:position [3 0 6] :normal [3 3 6]}
                          (for [[shared vs] by-shared]
                            [(count vs) {:shared  [:i shared]}]))))))

(defn set-shared!
  [csg shared]
  (doseq [p (.-polygons csg)]
    (aset p "shared" shared))
  csg)
