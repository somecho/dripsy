(in-package #:dripsy)


;; POINTS CACHE


(defvar *unit-triangle-points* (polygon-points 3 1.0))
(defvar *unit-square-points* (polygon-points 4 1.0))
(defvar *unit-pentagon-points* (polygon-points 5 1.0))
(defvar *unit-hexagon-points* (polygon-points 6 1.0))


(defun get-unit-polygon-points (n)
  (case n
    (3 *unit-triangle-points*)
    (4 *unit-square-points*)
    (5 *unit-pentagon-points*)
    (6 *unit-hexagon-points*)))


(defun greater-than-two-p (n) (> n 2))
(declaim (ftype (function ((satisfies greater-than-two-p) number)
                          points-array) polygon-points))
(defun polygon-points (n radius)
  "Returns a flat array of points of a N-sided polygon with a radius of
RADIUS."
  (let* ((tau (* PI 2.0))
         (theta (/ tau (coerce n 'float)))
         (vertices (make-array (* 3 n))))
    (loop for i from 0 below n
          for x = (* (sin (* i theta)) radius)
          for y = (* (cos (* i theta)) radius)
          do (setf (aref vertices (* i 3)) (coerce x 'single-float))
          do (setf (aref vertices (+ (* i 3) 1)) (coerce y 'single-float))
          do (setf (aref vertices (+ (* i 3) 2)) 0.0))
    vertices))


(defun unit-polygon (n)
  "Returns a flat array of 3d-points of a N-sided polygon with a radius of 1."
  (polygon-points n 1.0))


(defun triangle-points (&optional (size 1.0))
  "Returns a flat array of points representing the 3 sides of a triangle with a
radius of SIZE, radius being the length from the center to the point."
  (vec-mul-scalar *unit-triangle-points* size))

