;;;; PRIMITIVES
;;;; GL draw calls for primitive shapes. This handles rendering.

(in-package #:dripsy)

;;; 2D PRIMITIVES
;;; GL draw calls for 2D primitive shapes including:
;;; - point
;;; - points
;;; - line
;;; - eq-tri
;;; - tri
;;; - rect
;;; - polygon
;;; - circle
;;; - bezier

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
    (let* ((scaled (vecs* *unit-triangle-points* radius))
           (transposed (transpose-points-array scaled x y)))
      (write-array-buffer *renderer* transposed))
    (if use-fill?
        (gl:draw-arrays :triangles 0 3)
        (gl:draw-arrays :line-loop 0 3))))



(declaim (ftype (function (gl-num gl-num gl-num gl-num gl-num gl-num)) tri))
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



(declaim (ftype (function (gl-num gl-num gl-num gl-num)) rect))
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
             (scaled (vecs* unit-points-array radius))
             (transposed (transpose-points-array scaled x y)))
        (write-array-buffer *renderer* transposed)
        (if (use-fill? *renderer*)
            (gl:draw-arrays :triangle-fan 0 sides)
            (gl:draw-arrays :line-loop 0 sides)))))



(defun circle (x y radius)
  "Draws a circle at (x,y) with size of RADIUS."
  (before-render *renderer*)
  (let* ((points (circle-cache *renderer*))
         (scaled (vecs* points radius))
         (transposed (transpose-points-array scaled x y)))
    (write-array-buffer *renderer* transposed)
    (if (use-fill? *renderer*)
        (gl:draw-arrays :triangle-fan 0 (circle-resolution *renderer*))
        (gl:draw-arrays :line-loop 0 (circle-resolution *renderer*)))))



(defun bezier (x0 y0 x1 y1 x2 y2 x3 y3 &optional (res 32))
  "Draws a cubic bezier curve from point (x0, y0) to (x3,y3) with control points
(x1, y1) and (x2, y2). The default resolution of the line is 32."
  (let ((vertices (loop for i from 0 to res
                        for v = (coerce (/ i res) 'single-float)
                        for p = (bezier-lerp x0 y0
                                             x1 y1
                                             x2 y2
                                             x3 y3
                                             v)
                        collect (aref p 0)
                        collect (aref p 1))))
    (apply #'line vertices)))



(defun bezier* (p0 p1 p2 p3 &optional (res 32))
  "Draws a cubic bezier curve from P0 to P3 with control points P1 and P2, where
each point is a 2D simple-vector. The default resolution of the line is 32."
  (bezier (aref p0 0) (aref p0 1)
          (aref p1 0) (aref p1 1)
          (aref p2 0) (aref p2 1)
          (aref p3 0) (aref p3 1) res))
