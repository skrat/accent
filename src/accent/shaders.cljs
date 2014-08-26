(ns accent.shaders
  (:require [accent.arrays :as ta]
            [accent.const :as GL]
            [accent.context :refer [gl]]
            [accent.textures :as textures]))

(defprotocol IProgram
  (get-attribute-location [this attribute])
  (get-uniform-location   [this uniform])
  (get-program            [this]))

(deftype Program [program cache]
  IProgram
  (get-attribute-location [_ attribute]
    (when-not (aget cache attribute)
      (let [loc (.getAttribLocation gl program attribute)]
        (when (>= loc 0)
          (aset cache attribute loc)
          (.enableVertexAttribArray gl loc))))
    (aget cache attribute))
  (get-uniform-location [_ uniform]
    (when-not (aget cache uniform)
      (aset cache uniform (.getUniformLocation gl program uniform)))
    (aget cache uniform))
  (get-program [_] program))

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
     (Program. program #js {}))))

(defn use!
  [program]
  (.useProgram gl (get-program program)))

(defn set-uniform!
  [program attr type value]
  (let [loc (get-uniform-location program (name attr))
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
      :samp (let [[unit texture] value]
              (textures/bind! texture unit)
              ;; FIXME multiple textures!
              (.uniform1i gl loc unit))
      (throw (js/Error. (str "Unknown uniform type " type))))))

(defn set-uniforms!
  [program uniforms]
  (doseq [[attr [type value]] uniforms]
    (set-uniform! program attr type value)))
