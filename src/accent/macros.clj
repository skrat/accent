(ns accent.macros
  (:require [clojure.java.io :as io]
            [clojure.string :refer [trim]]))

(defn load-lines
"Turns relative path into lines sequence, resolving any found `#require`
 statements. `#require` supports relative path to the source file."
  [path]
  (flatten
    (with-open [rdr (io/reader path)]
      (for [line (doall (line-seq rdr))]
        (let [[[_ pth]] (re-seq #"^\s*?#require\s+([^\s]+)" line)]
          (if pth
            (try
              (load-lines pth)
              (catch java.io.FileNotFoundException e
                (load-lines (-> (io/file path)
                                (.getParent)
                                (str "/" pth)))))
            line))))))

(defmacro load-program!
"Creates WebGL program from path to the shader and optional headers
 to prepend. NOTE: it expect WebGL context to be bound in `accent.context`"
  [relative-uri header]
  (let [origin [(str "// source: " relative-uri)]
        source (load-lines relative-uri)
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
