(in-package #:dripsy)


;; RENDERER CLASS


(declaim (type (or null renderer) *renderer*))
(defvar *renderer* nil
  "The global renderer instance used by Dripsy to draw in the current GL
  context. It is initialized when an instance of the Dripsy app is created.")


(defclass renderer ()
  ((vertices
    :initform #()
    :accessor vertices)
   (indices
    :initform #()
    :accessor indices)
   (col
    :initform #(1.0 1.0 1.0 1.0)
    :accessor col)
   (colors
    :initform #()
    :accessor colors)
   (vertex-buffer-location
    :initform nil
    :accessor vertex-buffer-location)
   (vertex-attrib-location
    :initform nil
    :accessor vertex-attrib-location)
   (default-shader
    :initform nil
    :accessor default-shader)
   (width
    :initform nil
    :initarg :width
    :accessor width)
   (height
    :initform nil
    :initarg :height
    :accessor height)
   (model-matrix
    :initform (kit.glm:identity-matrix)
    :accessor model-matrix)
   (view-matrix
    :initform (kit.glm:identity-matrix)
    :accessor view-matrix)
   (projection-matrix
    :initform nil
    :accessor projection-matrix)
   (mvp-matrix
    :initform nil
    :accessor mvp-matrix)))


(defmethod initialize-instance :after ((renderer renderer) &key)
  (with-accessors ((vertex-buffer-location vertex-buffer-location)
                   (vertex-attrib-location vertex-attrib-location)
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
    (setf vertex-attrib-location (gl:gen-vertex-array))))


(defmethod bind-array-buffer ((renderer renderer))
  "Binds the renderer's VBO if it isn't already bound."
  (with-accessors ((vertex-buffer-location vertex-buffer-location))
      renderer
    (let ((current-array-buffer (gl:get-integer :array-buffer-binding) ))
      (unless (eq vertex-buffer-location current-array-buffer)
        (gl:bind-buffer :array-buffer vertex-buffer-location)))))


(defmethod bind-vertex-array ((renderer renderer))
  "Binds the renderer's VAO if it isn't already bound."
  (with-accessors ((vertex-attrib-location vertex-attrib-location))
      renderer
    (let ((current-attrib (gl:get-integer :vertex-array-binding) ))
      (unless (eq vertex-attrib-location current-attrib)
        (gl:bind-vertex-array vertex-attrib-location)))))


(defmethod use-default-shader ((renderer renderer))
  "Uses the renderer's default shaders and sets its default uniforms if it isn't
already done."
  (with-accessors ((default-shader default-shader)
                   (mvp-matrix mvp-matrix))
      renderer
    (let ((current-shader (gl:get-integer :current-program)))
      (unless (eq current-shader default-shader)
        (gl:use-program default-shader)
        (let ((mvp-loc (gl:get-uniform-location default-shader "modelViewProjectionMatrix")))
          (gl:uniform-matrix-4fv mvp-loc mvp-matrix))))))


(declaim (ftype (function (renderer points-array)) write-array-buffer))
(defmethod write-array-buffer ((renderer renderer) data)
  "Uploads the vertex attributes position and color to OpenGL if the vertex
position data has changed."
  (with-accessors ((vertices vertices)
                   (col col)
                   (colors colors))
      renderer
    (unless (equalp data vertices)
      (setf vertices data)
      (setf colors (flat-repeat col (array-total-size data)))
      (let ((vertex-data (interlace-arrays vertices colors 3 4)))
        (gl:buffer-data :array-buffer :dynamic-draw
                        (gen-gl-array-buffer vertex-data))))))


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


(declaim (ftype (function (u8 u8 u8 u8)) set-color))
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


(declaim (ftype (function (gl-num gl-num gl-num)) eq-tri))
(defun eq-tri (x y radius)
  "Draws an equilateral triangle with center (X,Y). The length from the center
to its points is RADIUS."
  (with-accessors ((vertices vertices)
                   (vertex-attrib-location vertex-attrib-location)
                   (default-shader default-shader))
      *renderer*
    (before-render *renderer*)
    (let* ((scaled (vec-mul-scalar *unit-triangle-points* radius))
           (transposed (transpose-points-array scaled x y))
           (current-bound-attrib (gl:get-integer :vertex-array-binding)))
      (write-array-buffer *renderer* transposed))
    (gl:draw-arrays :triangles 0 3)))