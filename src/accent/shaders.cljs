(ns accent.shaders
  (:require [accent.constants :as GL]
            [accent.context :refer [gl]]
            [accent.textures :as textures]))

(defn get-shader-info-log
"Return the information log for a shader object."
  [shader]
  (.getShaderInfoLog gl shader))

(defn get-program-info-log
"Return the information log for a program object."
  [program]
  (.getProgramInfoLog gl program))

(defn get-shader-parameter
"Return a parameter from a shader object."
  [shader pname]
  (.getShaderParameter gl shader pname))

(defn get-program-parameter
"Return a parameter from a program object."
  [program pname]
  (.getProgramParameter gl program pname))

(def get-attribute-location
"Return the location of an attribute variable."
  (memoize (fn
  [program attrib-name]
  (let [loc (.getAttribLocation gl program attrib-name)]
    (when (>= loc 0)
      (.enableVertexAttribArray gl loc))
    loc))))

(def get-uniform-location
"Return the location of a uniform variable."
  (memoize (fn
  [program uniform-name]
  (.getUniformLocation gl program uniform-name))))

(defn check-shader
"Checks for shader (compilation) error and throws if one is found."
  [shader]
  (let [status (get-shader-parameter shader GL/compile-status)]
    (when (not status)
      (throw (js/Error. (str "Shader failed to compile: "
                        (get-shader-info-log shader)))))
  shader))

(defn check-program
"Checks for program (linking) error and throws if one is found."
  [program]
  (let [status (get-program-parameter program GL/link-status)]
    (when (not status)
      (throw (js/Error. (str "Program failed to link: "
                        (get-program-info-log program)))))
    program))

(defn create-shader!
"Creates a shader object from given GLSL source,
 and checks for compilation errors."
  [type source]
  (let [shader (.createShader gl type)]
    (.shaderSource gl shader source)
    (.compileShader gl shader)
    (check-shader shader)
    shader))

(defn create-program!
"Creates a program object and checks for linking errors."
  ([vs-source fs-source]
   (create-program! [
     (create-shader! GL/vertex-shader vs-source)
     (create-shader! GL/fragment-shader fs-source)]))
  ([shaders]
   (let [program (.createProgram gl)]
     (dorun (map (fn [shader] (.attachShader gl program shader)) shaders))
     (.linkProgram gl program)
     (check-program program)
     program)))

(defn use!
  [program]
  (.useProgram gl program))

(defn memoize-once
"Like memoize, but remembers just one last call."
  [f]
  (let [mem (atom {})]
    (fn [& args]
      (if-let [e (find @mem args)]
        (val e)
        (let [ret (apply f args)]
          (reset! mem {args ret})
          ret)))))

(def set-uniform!
  (memoize-once
    (fn [program name type value]
      (let [loc (get-uniform-location program name)
            [x y z w] (when (sequential? value) value)]
        (case type
          :i    (.uniform1i        gl loc value)
          :f    (.uniform1f        gl loc value)
          :val2 (.uniform2f        gl loc x y)
          :val3 (.uniform3f        gl loc x y z)
          :val4 (.uniform3f        gl loc x y z w)
          :vec2 (.uniform2fv       gl loc value)
          :vec3 (.uniform3fv       gl loc value)
          :vec4 (.uniform4fv       gl loc value)
          :mat3 (.uniformMatrix3fv gl loc false value)
          :mat4 (.uniformMatrix4fv gl loc false value)
          :samp (do
                  (textures/bind! value)
                  ;; FIXME multiple textures!
                  (.uniform1i gl loc 0))
          (throw (js/Error. (str "Unknown type " type))))))))

(defn set-uniforms!
  [program uniforms]
  (doseq [[name type value] uniforms]
    (let [loc (get-uniform-location program name)
          [x y z w] (when (sequential? value) value)]
      (case type
        :i    (.uniform1i        gl loc value)
        :f    (.uniform1f        gl loc value)
        :val2 (.uniform2f        gl loc x y)
        :val3 (.uniform3f        gl loc x y z)
        :val4 (.uniform3f        gl loc x y z w)
        :vec2 (.uniform2fv       gl loc value)
        :vec3 (.uniform3fv       gl loc value)
        :vec4 (.uniform4fv       gl loc value)
        :mat3 (.uniformMatrix3fv gl loc false value)
        :mat4 (.uniformMatrix4fv gl loc false value)
        :samp (do
                (textures/bind! value)
                ;; FIXME multiple textures!
                (.uniform1i gl loc 0))
        (throw (js/Error. (str "Unknown type " type)))))))