(in-package #:dripsy)

(defun vec-mul-scalar (vec f)
  "Returns an array of all elements in VEC multiplied by F"
  (declare (type (simple-array (or single-float fixnum)) vec))
  (declare (type (or float fixnum) f))
  (let* ((vec-length (length vec))
         (output (make-array vec-length)))
    (loop for i from 0 below vec-length
          do (setf (aref output i) (* f(aref vec i))))
    output))
