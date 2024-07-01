(in-package #:feedback)

(define-condition polygon-has-less-than-three-sides (error) ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "A polygon must have more than 2 sides."))))

(defun polygon (n radius)
  "Returns a flat array of points of a N-sided polygon with a radius of
RADIUS."
  (when (< n 3)
    (error 'polygon-has-less-than-three-sides :sides n))
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
  (polygon n 1.0))

(defvar *unit-triangle-points* (polygon 3 1.0))

(defun triangle-points (&key (size 1.0))
  "Returns a flat array of points representing the 3 sides of a triangle with a
radius of SIZE, radius being the length from the center to the point."
  (vec-mul-scalar *unit-triangle-points* size))

