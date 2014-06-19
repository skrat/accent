(ns accent.solids
  (:require [accent.arrays :as ta]
            [accent.drawables :as drawables]
            [accent.tesselate :refer [tesselate]]))

(defn vec->clj [v]
  [(.-x v) (.-y v) (.-z v)])

(defn poly->clj [poly]
  (let [  vertices (.-vertices poly)
         positions (map vec->clj (map #(.-pos %) vertices))
           normals (map vec->clj (map #(.-normal %) vertices))]
    (map vector positions (map vector normals))))

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

(defn csg->drawable [csg]
  (let [    faces (map poly->clj (.-polygons csg))
        triangles (apply concat (triangulate faces))
        positions (map first triangles)
          normals (map #(first (second %)) triangles)
         vertices (apply concat (interleave positions normals))]

    (drawables/create! (ta/float32 vertices) (count positions)
                       [[:position 3 0 6] [:normal 3 3 6]])))
