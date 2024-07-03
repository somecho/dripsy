(in-package #:dripsy)


;; TYPES


(deftype points-array ()
  "An array of gl-nums that has a size which is a multiple of 3"
  `(and (simple-array gl-num) (satisfies triplet-array-p)))


(deftype gl-num ()
  "An integer or an f32. This is for OpenGL to make sure the numbers fit."
  `(or fixnum single-float))


;; TYPE PREDICATES


(defun triplet-array-p (array)
  "Returns T if array has a size that is a multiple of 3."
  (zerop (mod (array-total-size array) 3)))


;; VECTOR/ARRAY OPERATIONS


(defun vec-mul-scalar (vec f)
  "Returns an array of all elements in VEC multiplied by F"
  (declare (type (simple-array (or single-float fixnum)) vec))
  (declare (type (or float fixnum) f))
  (let* ((vec-length (length vec))
         (output (make-array vec-length)))
    (loop for i from 0 below vec-length
          do (setf (aref output i) (* f (aref vec i))))
    output))


(declaim (ftype (function (points-array gl-num gl-num (or gl-num null))
                          points-array)
                transpose-points-array))


(defun transpose-points-array (array x y &optional (z 0))
  "Returns a new points array where the x, y and z components are transposed by
the arguments X, Y and Z."
  (let* ((n (/ (array-total-size array) 3))
         (output (make-array (* n 3))))
    (loop for i from 0 below n
          for ix = (* i 3)
          for iy = (+ (* i 3) 1)
          for iz = (+ (* i 3) 2)
          do (setf (aref output ix)
                   (coerce (+ (aref array ix) x) 'single-float))
          do (setf (aref output iy)
                   (coerce (+ (aref array iy) y) 'single-float))
          do (setf (aref output iz)
                   (coerce (+ (aref array iz) z) 'single-float)))
    output))
