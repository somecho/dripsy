(in-package #:dripsy)


(defvar *renderer*)


(defclass renderer ()
  ((vertices
    :initform #()
    :accessor vertices)
   (indices
    :initform #()
    :accessor indices)
   (fill-col
    :initform #(1.0 1.0 1.0)
    :accessor fill-col)
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
  (with-accessors ((vertex-buffer-location vertex-buffer-location))
      renderer
    (let ((current-array-buffer (gl:get-integer :array-buffer-binding) ))
      (unless (eq vertex-buffer-location current-array-buffer)
        (gl:bind-buffer :array-buffer vertex-buffer-location)))))


(defmethod bind-vertex-array ((renderer renderer))
  (with-accessors ((vertex-attrib-location vertex-attrib-location))
      renderer
    (let ((current-attrib (gl:get-integer :vertex-array-binding) ))
      (unless (eq vertex-attrib-location current-attrib)
        (gl:bind-vertex-array vertex-attrib-location)))))


(defmethod use-default-shader ((renderer renderer))
  (with-accessors ((default-shader default-shader)
                   (mvp-matrix mvp-matrix))
      renderer
    (let ((current-shader (gl:get-integer :current-program)))
      (unless (eq current-shader default-shader)
        (gl:use-program default-shader)
        (let ((mvp-loc (gl:get-uniform-location default-shader "modelViewProjectionMatrix")))
          (gl:uniform-matrix-4fv mvp-loc mvp-matrix))))))


(defmethod write-array-buffer ((renderer renderer) data)
  (with-accessors ((vertices vertices))
      renderer
    (unless (equalp data vertices)
      (setf vertices data)
      (gl:buffer-data :array-buffer :dynamic-draw
                      (gen-gl-array-buffer data)))))


(defmethod before-render ((renderer renderer))
  (use-default-shader renderer)
  (bind-array-buffer renderer)
  (bind-vertex-array renderer)
  (gl:enable-vertex-attrib-array 0)
  (gl:vertex-attrib-pointer 0 3 :float :false 0 0))


(defun triangle (x y radius)
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
