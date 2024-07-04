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
   (index-buffer-location
    :initform nil
    :accessor index-buffer-location)
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
    :accessor mvp-matrix)
   (use-fill?
    :initform t
    :accessor use-fill?)
   (circle-resolution
    :initform 32
    :accessor circle-resolution)
   (circle-cache
    :initform (unit-polygon 32)
    :accessor circle-cache)))


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


(defmethod write-index-buffer ((renderer renderer) data)
  (with-accessors ((indices indices)
                   (index-buffer-location index-buffer-location))
      renderer
    (unless (equalp data indices)
      (setf indices data)
      (gl:bind-buffer :element-array-buffer index-buffer-location)
      (gl:buffer-data :element-array-buffer :dynamic-draw
                      (gen-gl-array-buffer data :type :unsigned-int)))))


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


;; Renderer State Functions


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


(defun no-fill! ()
  "Tells the renderer to draw shapes without fill."
  (setf (use-fill? *renderer*) nil))


(defun use-fill! ()
  "Tells the renderer to draw shapes with fill."
  (setf (use-fill? *renderer*) t))


(declaim (ftype (function ((and fixnum greater-than-two-p))) set-circle-resolution))
(defun set-circle-resolution (resolution)
  "By default, a circle has 32 points. To specify a how many points the renderer
will draw circles with, use this method."
  (with-accessors ((circle-resolution circle-resolution)
                   (circle-cache circle-cache))
      *renderer*
    (unless (eq resolution circle-resolution)
      (setf circle-resolution resolution)
      (setf circle-cache (unit-polygon resolution)))))


;; 2D Primitive Draw Calls


(defun point (x y &optional (z 0.0))
  "Draws a single point at (x,y)."
  (before-render *renderer*)
  (write-array-buffer *renderer* `#(,(coerce x 'single-float)
                                    ,(coerce y 'single-float)
                                    ,(coerce z 'single-float)))
  (gl:draw-arrays :points 0 1))


(defun points (x y &rest points)
  "Draws either a single point or multiple points. More points can be given in
the &rest argument as flat pairs."
  (before-render *renderer*)
  (let ((num-points (-> (length points)(/ 2)(+ 1)))
        (pts-array (points-array-from x y points)))
    (write-array-buffer *renderer* pts-array)
    (gl:draw-arrays :points 0 num-points)))


(defun line (x1 y1 x2 y2 &rest points)
  "Draws a line with points (x1,y1) to (x2,y2). More points can be given in the
&rest arguments but must be given in pairs."
  (before-render *renderer*)
  (let ((num-vertices (-> (length points) (/ 2) (+ 2)))
        (line-array (line-array-from x1 y1 x2 y2 points)))
    (write-array-buffer *renderer* line-array)
    (gl:draw-arrays :line-strip 0 num-vertices)))


(declaim (ftype (function (gl-num gl-num gl-num)) eq-tri))
(defun eq-tri (x y radius)
  "Draws an equilateral triangle with center (X,Y). The length from the center
to its points is RADIUS."
  (with-accessors ((use-fill? use-fill?))
      *renderer*
    (before-render *renderer*)
    (let* ((scaled (vec-mul-scalar *unit-triangle-points* radius))
           (transposed (transpose-points-array scaled x y)))
      (write-array-buffer *renderer* transposed))
    (if use-fill?
        (gl:draw-arrays :triangles 0 3)
        (gl:draw-arrays :line-loop 0 3))))


(declaim (ftype (function (gl-num gl-num gl-num gl-num gl-num gl-num) tri)))
(defun tri (x1 y1 x2 y2 x3 y3)
  "Draws a triangle with points (x1,y1), (x2,y2) and (x3,y3)."
  (before-render *renderer*)
  (let ((tri-array `#(,(coerce x1 'single-float)
                      ,(coerce y1 'single-float) 0.0
                      ,(coerce x2 'single-float)
                      ,(coerce y2 'single-float) 0.0
                      ,(coerce x3 'single-float)
                      ,(coerce y3 'single-float) 0.0)))
    (write-array-buffer *renderer* tri-array)
    (if (use-fill? *renderer*)
        (gl:draw-arrays :triangles 0 3)
        (gl:draw-arrays :line-loop 0 3))))



(defun rect (x y w h)
  "Draws a rectangle with the top left corner at (x,y) of width W and height
H."
  (before-render *renderer*)
  (let* ((rect-array `#(,(coerce x 'single-float)
                        ,(coerce y 'single-float) 0.0
                        ,(coerce (+ x w) 'single-float)
                        ,(coerce y 'single-float) 0.0
                        ,(coerce (+ x w) 'single-float)
                        ,(coerce (+ y h) 'single-float) 0.0
                        ,(coerce x 'single-float)
                        ,(coerce (+ y h) 'single-float) 0.0)))
    (write-array-buffer *renderer* rect-array)
    (if (use-fill? *renderer*)
        (gl:draw-arrays :triangle-fan 0 4)
        (gl:draw-arrays :line-loop 0 4))))


(defun polygon (x y radius sides)
  "Draws a polygon at (x,y) with SIDES being the number of sides it has. The
radius is the length from the center to its points."
  (before-render *renderer*)
  (if (= sides 3)
      (eq-tri x y radius)
      (let* ((unit-points-array (if (< sides 7)
                                    (get-unit-polygon-points sides)
                                    (unit-polygon sides)))
             (scaled (vec-mul-scalar unit-points-array radius))
             (transposed (transpose-points-array scaled x y)))
        (write-array-buffer *renderer* transposed)
        (if (use-fill? *renderer*)
            (gl:draw-arrays :triangle-fan 0 sides)
            (gl:draw-arrays :line-loop 0 sides)))))


(defun circle (x y radius)
  "Draws a circle at (x,y) with size of RADIUS."
  (before-render *renderer*)
  (let* ((points (circle-cache *renderer*))
         (scaled (vec-mul-scalar points radius))
         (transposed (transpose-points-array scaled x y)))
    (write-array-buffer *renderer* transposed)
    (if (use-fill? *renderer*)
        (gl:draw-arrays :triangle-fan 0 (circle-resolution *renderer*))
        (gl:draw-arrays :line-loop 0 (circle-resolution *renderer*)))))
