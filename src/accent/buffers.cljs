(ns accent.buffers
  (:require [accent.context :refer [*gl*]]
            [accent.symbols :as GL]))

(defn bind!
  [target buffer]
  (.bindBuffer *gl* target buffer))

(defn unbind!
  [target]
  (.bindBuffer *gl* target nil))

(defn data!
  [target data usage]
  (.bufferData *gl* target data usage))

(defn create!
  [data target usage]
  (let [buffer (.createBuffer *gl*)]
    (bind! target buffer)
    (data! target data usage)
    buffer))

(defn create-render! []
  (.createRenderbuffer *gl*))

(defn bind-render!
  [buffer]
  (.bindRenderbuffer *gl* GL/renderbuffer buffer))

(defn unbind-render! []
  (.bindRenderbuffer *gl* GL/renderbuffer nil))

(defn set-storage!
  [format width height]
  (.renderbufferStorage *gl* GL/renderbuffer format width height))

(defn create-frame! []
  (.createFramebuffer *gl*))

(defn bind-frame!
  [buffer]
  (.bindFramebuffer *gl* GL/framebuffer buffer))

(defn unbind-frame! []
  (.bindFramebuffer *gl* GL/framebuffer nil))

(defn check-frame
  [buffer]
  (let [result (.checkFramebufferStatus *gl* GL/framebuffer)
        msg    (case result
                     GL/framebuffer-unsupported
                       "Framebuffer is unsupported"
                     GL/framebuffer-incomplete-attachment
                       "Framebuffer has incomplete attachment"
                     GL/framebuffer-incomplete-dimensions
                       "Framebuffer has incomplete dimensions"
                     GL/framebuffer-incomplete-missing-attachment
                       "Framebuffer has incomplete or missing attachment"
                     nil)]
    (when msg (throw (js/Error. msg)))))

(defn attach-color!
  [{:keys [target handle]}]
  (.framebufferTexture2D *gl*
                         GL/framebuffer
                         GL/color-attachment0
                         target handle 0))

(defn attach-depth!
  [buffer]
  (.framebufferRenderbuffer *gl*
                            GL/framebuffer
                            GL/depth-attachment
                            GL/renderbuffer
                            buffer))

(defn clear-color!
  [r g b a]
  (.clearColor *gl* r g b a)
  (.clear *gl* GL/color-buffer-bit))

(defn clear-depth!
  [depth]
  (.clearDepth *gl* depth)
  (.clear *gl* GL/depth-buffer-bit))

(defn clear-both!
  [r g b a depth]
  (.clearColor *gl* r g b a)
  (.clearDepth *gl* depth)
  (.clear *gl* (bit-or GL/color-buffer-bit GL/depth-buffer-bit)))
