(ns accent.macros
  (:require [clojure.java.io :as io]
            [clojure.string :refer [trim]]))

(defmacro load-program!
  [relative-uri header]
  (let [origin [(str "// source: " relative-uri)]
        source (with-open [rdr (io/reader relative-uri)]
                 (doall (line-seq rdr)))
        common (vec (take-while #(not= "vertex:" (trim %)) source))
        vertex (vec (->> source (drop-while #(not= "vertex:" (trim %)))
                                (take-while #(not= "fragment:" (trim %)))
                                (rest)))
        fragment (vec (rest (drop-while #(not= "fragment:" %) source)))]

    `(accent.shaders/create-program!
      (apply + (interpose "\n" (concat ~origin ~header ~common ~vertex)))
      (apply + (interpose "\n" (concat ~origin ~header ~common ~fragment))))))

(defmacro with-node
  [node & body]
  `(do (accent.nodes/begin! ~node) ~@body
       (accent.nodes/end! ~node)))
