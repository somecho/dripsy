;;;; RENDERER
;;;; This file contains the renderer class, which handles OpenGL state for
;;;; drawing to the screen.

(in-package #:dripsy)

;;; RENDERER CLASS



(declaim (type (or null renderer) *renderer*))
(defvar *renderer* nil
  "The global renderer instance used by Dripsy to draw in the current GL
  context. It is initialized when an instance of the Dripsy app is created.")



(defclass renderer ()
  ((vertices :initform #() :accessor vertices)
   (indices :initform #() :accessor indices)
   (col :initform #(1.0 1.0 1.0 1.0) :accessor col)
   (colors :initform #() :accessor colors)
   (vertex-buffer-location :initform nil :accessor vertex-buffer-location)
   (vertex-attrib-location :initform nil :accessor vertex-attrib-location)
   (index-buffer-location :initform nil :accessor index-buffer-location)
   (default-shader :initform nil :accessor default-shader)
   (width :initform nil :initarg :width :accessor width)
   (height :initform nil :initarg :height :accessor height)
   (matrix-stack :initform nil :accessor matrix-stack)
   (model-matrix :initform (kit.glm:identity-matrix) :accessor model-matrix)
   (view-matrix :initform (kit.glm:identity-matrix) :accessor view-matrix)
   (projection-matrix :initform nil :accessor projection-matrix)
   (mvp-matrix :initform nil :accessor mvp-matrix)
   (use-fill? :initform t :accessor use-fill?)
   (circle-resolution :initform 32 :accessor circle-resolution)
   (circle-cache :initform (unit-polygon 32) :accessor circle-cache)))



(defmethod initialize-instance :after ((renderer renderer) &key)
  (with-accessors ((vertex-buffer-location vertex-buffer-location)
                   (vertex-attrib-location vertex-attrib-location)
                   (index-buffer-location index-buffer-location)
                   (default-shader default-shader)
                   (model-matrix model-matrix)
                   (view-matrix view-matrix)
                   (projection-matrix projection-matrix)
                   (mvp-matrix mvp-matrix))
      renderer
    (setf projection-matrix (kit.glm:ortho-matrix 0 (width renderer) 0 (height renderer) -1 1))
    (setf mvp-matrix (kit.glm:matrix* model-matrix view-matrix projection-matrix))
    (setf default-shader (create-default-shader-program))
    (setf vertex-buffer-location (gl:gen-buffer))
    (setf index-buffer-location (gl:gen-buffer))
    (setf vertex-attrib-location (gl:gen-vertex-array))))



(defmethod use-default-shader ((renderer renderer))
  "Uses the renderer's default shaders and sets its default uniforms if it isn't
already done."
  (with-accessors ((default-shader default-shader)
                   (mvp-matrix mvp-matrix))
      renderer
    (let ((current-shader (gl:get-integer :current-program)))
      (unless (eq current-shader default-shader)
        (gl:use-program default-shader))
      (let ((mvp-loc (gl:get-uniform-location default-shader "modelViewProjectionMatrix")))
        (gl:uniform-matrix-4fv mvp-loc mvp-matrix)))))



(defmethod before-render ((renderer renderer))
  (use-default-shader renderer)
  (bind-array-buffer renderer)
  (bind-vertex-array renderer)
  (let* ((fsize (cffi:foreign-type-size :float))
         (stride (* 7 fsize)))
    (gl:enable-vertex-attrib-array 0)
    (gl:vertex-attrib-pointer 0 3 :float :false stride 0)
    (gl:enable-vertex-attrib-array 1)
    (gl:vertex-attrib-pointer 1 4 :float :false stride (* 3  fsize))))



;;; RENDERER STATE FUNCTIONS



(declaim (ftype (function (u8 u8 u8 &optional u8)) set-color))
(defun set-color (r g b &optional (a 255.0))
  "Sets the color that the renderer will use to render the subsequent
geometries."
  (let* ((new-col `#(,(/ (coerce r 'single-float) 255.0)
                     ,(/ (coerce g 'single-float) 255.0)
                     ,(/ (coerce b 'single-float) 255.0)
                     ,(/ (coerce a 'single-float) 255.0))))
    (with-accessors ((col col)
                     (colors colors)
                     (vertices vertices))
        *renderer*
      (unless (equalp col new-col)
        (setf col new-col)
        (bind-array-buffer *renderer*)
        (setf colors (flat-repeat col (array-total-size vertices)))
        (let ((vertex-data (interlace-arrays vertices colors 3 4)))
          (gl:buffer-data :array-buffer
                          :dynamic-draw
                          (gen-gl-array-buffer vertex-data)))))))



(defun no-fill ()
  "Tells the renderer to draw shapes without fill."
  (setf (use-fill? *renderer*) nil))



(defun use-fill ()
  "Tells the renderer to draw shapes with fill."
  (setf (use-fill? *renderer*) t))



(defun set-circle-resolution (resolution)
  "By default, a circle has 32 points. To specify a how many points the renderer
will draw circles with, use this method."
  (with-accessors ((circle-resolution circle-resolution)
                   (circle-cache circle-cache))
      *renderer*
    (unless (eq resolution circle-resolution)
      (setf circle-resolution resolution)
      (setf circle-cache (unit-polygon resolution)))))



(defun save-screen (w h filename)
  (let* ((num-pixels (* w h))
         (size (* num-pixels 4))
         (png (make-instance 'zpng:pixel-streamed-png
                             :color-type :truecolor-alpha
                             :width w
                             :height h)))
    (cffi:with-foreign-object (array '%gl:ubyte size)
      (%gl:read-pixels 0 0 w h :rgba :unsigned-byte array)
      (with-open-file (stream filename
                              :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create
                              :element-type '(unsigned-byte 8))
        (zpng:start-png png stream)
        (loop for y from 0 below h
              do (loop for x from 0 below w
                       for i = (+ (* (- h y 1) w) x)
                       for r = (* i 4)
                       for g = (+ 1 (* i 4))
                       for b = (+ 2 (* i 4))
                       for a = (+ 3 (* i 4))
                       do (zpng:write-pixel
                           (list
                            (cffi:mem-aref array '%gl:ubyte r)
                            (cffi:mem-aref array '%gl:ubyte g)
                            (cffi:mem-aref array '%gl:ubyte b)
                            (cffi:mem-aref array '%gl:ubyte a)) png)))
        (zpng:finish-png png)))))



(defun clear ()
  "Clears the color buffer bit and the depth buffer bit"
  (gl:clear :color-buffer-bit :depth-buffer-bit))



(defun background (r g b &optional (a 255.0))
  "Sets the background color. Values range from 0-255."
  (let ((red (coerce r 'single-float))
        (green (coerce g 'single-float))
        (blue (coerce b 'single-float))
        (alpha (coerce a 'single-float)))
    (gl:clear-color (/ red 255.0)
                    (/ green 255.0)
                    (/ blue 255.0)
                    (/ alpha 255.0))
    (gl:clear :color-buffer)))

