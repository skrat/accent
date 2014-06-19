(ns accent.context)

(def ^:dynamic gl
"Dynamic binding for WebGL context."
  nil)

(def default-attributes {:alpha true
                         :depth true,
                         :stencil false,
                         :antialias true,
                         :premultiplied-alpha true,
                         :preserve-drawing-buffer false})

(defn attributes->js
  [{:keys [alpha
           depth
           stencil
           antialias
           premultiplied-alpha
           preserve-drawing-buffer]}]
  (clj->js {:alpha alpha
            :depth depth
            :stencil stencil
            :antialias antialias
            :premultipliedAplha premultiplied-alpha
            :preserveDrawingBuffer preserve-drawing-buffer}))

(defn get-context
"Gets a WebGL context from a canvas element.
`context-attributes` may be a map in the following form:

    {:alpha
     :depth
     :stencil
     :antialias
     :premultiplied-apha
     :preserve-drawing-buffer}

If you don't specify any key, the default value is assumed.
For further information on context creation parameters see

[WebGLContextAttributes]
(https://www.khronos.org/registry/webgl/specs/1.0.2/#WEBGLCONTEXTATTRIBUTES);
"
  ([canvas]
   (get-context canvas {} []))
  ([canvas attributes extensions]
   (let [js-attrs (attributes->js (merge default-attributes attributes))
         context  (or (.getContext canvas "webgl" js-attrs)
                      (.getContext canvas "experimental-webgl" js-attrs))]
     (when (not context)
       (throw (js/Error. "WebGL not supported")))
     (doseq [ext extensions] (.getExtension context ext))
     context)))
