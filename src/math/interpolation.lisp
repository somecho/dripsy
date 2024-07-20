;;;; INTERPOLATION

(in-package #:dripsy.math)



;;; BEZIER INTERPOLATION



(defun bezier-lerp* (p0 p1 p2 p3 v)
  "Cubic bezier interpolation. Using value V (0-1), interpolates between points
P0 and P3, with P1 and P2 being control points. Each point is a simple-vector.
Calculates this using the polynomial form."
  (let ((c0 p0)
        (c1 (vecs*
             (vec+ (vecs* p0 -3) (vecs* p1 3))
             v))
        (c2 (vecs*
             (vec+ (vecs* p0 3) (vecs* p1 -6) (vecs* p2 3))
             (* v v)))
        (c3 (vecs*
             (vec+ (vecs* p0 -1) (vecs* p1 3) (vecs* p2 -3) p3)
             (* v v v))))
    (vec+ c0 c1 c2 c3)))



(defun bezier-lerp (x0 y0 x1 y1 x2 y2 x3 y3 v)
  "Cubic bezier interpolation using 2D Points. Using value V (0-1), interpolates
between points (x0, y0) and (x3, y3), with (x1, y1) and (x2, y2) being control
points."
  (bezier-lerp* `#(,x0 ,y0)
                `#(,x1 ,y1)
                `#(,x2 ,y2)
                `#(,x3 ,y3) v))
