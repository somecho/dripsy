(in-package #:dripsy)


(defun gen-gl-array-buffer (data &key (type :float))
  (let ((arr (gl:alloc-gl-array type (length data))))
    (dotimes (i (length data))
      (setf (gl:glaref arr i) (aref data i)))
    arr))
