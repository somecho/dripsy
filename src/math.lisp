(in-package #:dripsy)


;; TYPES


(deftype points-array ()
  "An array of gl-nums that has a size which is a multiple of 3"
  `(and (simple-array gl-num) (satisfies triplet-array-p)))


(deftype duplet-list ()
  `(and (or null (cons gl-num)) (satisfies duplet-list-p)))


(deftype gl-num ()
  "An integer or an f32. This is for OpenGL to make sure the numbers fit."
  `(or fixnum single-float))


;; TYPE PREDICATES


(defun greater-than-two-p (n) (> n 2))


(defun triplet-array-p (array)
  "Returns T if array has a size that is a multiple of 3."
  (zerop (mod (array-total-size array) 3)))


(defun duplet-list-p (l)
  (zerop (mod (length l) 2)))


;; VECTOR/ARRAY OPERATIONS


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


(defun flat-repeat (array ntimes)
  (let* ((size (array-total-size array))
         (output (make-array (* size ntimes))))
    (loop for i from 0 below ntimes
          do (loop for j from 0 below size
                   for index = (+ j (* i size))
                   do (setf (aref output index) (aref array j))))
    output))


(defun interlace-arrays (arr1 arr2 n1 n2)
  (let* ((size (/ (array-total-size arr1) n1))
         (output (make-array (* size (+ n1 n2)))))
    (loop for i from 0 below size
          do (loop for j from 0 below n1
                   for index = (+ j (* i (+ n1 n2)))
                   for ji = (+ (* i n1) j)
                   do (setf (aref output index) (aref arr1 ji)))
          do (loop for j from 0 below n2
                   for index = (+ j n1 (* i (+ n1 n2)))
                   for ji = (+ (* i n2) j)
                   do (setf (aref output index) (aref arr2 ji))))
    output))


;; (declaim (ftype (function (gl-num gl-num gl-num gl-num (or null duplet-list))
;;                           points-array) line-array-from))
(defun line-array-from (x1 y1 x2 y2 &optional points)
  "Used by the line draw call to convert points into an array that OpenGL can
use."
  (let* ((num-points (-> (length points) (/ 2) (+ 2)))
         (line-vertices (make-array (* 3 num-points))))
    (setf (aref line-vertices 0) (coerce x1 'single-float))
    (setf (aref line-vertices 1) (coerce y1 'single-float))
    (setf (aref line-vertices 2) 0.0)
    (setf (aref line-vertices 3) (coerce x2 'single-float))
    (setf (aref line-vertices 4) (coerce y2 'single-float))
    (setf (aref line-vertices 5) 0.0)
    (loop for i from 2 below num-points
          for ix = (* i 3)
          for iy = (-> (* i 3) (+ 1))
          for iz = (-> (* i 3) (+ 2))
          for jx = (-> i (- 2) (* 2))
          for jy = (-> i (- 2) (* 2) (+ 1))
          do (setf (aref line-vertices ix)
                   (coerce (nth jx points) 'single-float))
          do (setf (aref line-vertices iy)
                   (coerce (nth jy points) 'single-float))
          do (setf (aref line-vertices iz) 0.0))
    line-vertices))


(defun points-array-from (x y &optional points)
  (let* ((num-points (-> (length points) (/ 2) (+ 1)))
         (line-vertices (make-array (* 3 num-points))))
    (setf (aref line-vertices 0) (coerce x 'single-float))
    (setf (aref line-vertices 1) (coerce y 'single-float))
    (setf (aref line-vertices 2) 0.0)
    (loop for i from 1 below num-points
          for ix = (* i 3)
          for iy = (-> (* i 3) (+ 1))
          for iz = (-> (* i 3) (+ 2))
          for jx = (-> i (- 1) (* 2))
          for jy = (-> i (- 1) (* 2) (+ 1))
          do (setf (aref line-vertices ix)
                   (coerce (nth jx points) 'single-float))
          do (setf (aref line-vertices iy)
                   (coerce (nth jy points) 'single-float))
          do (setf (aref line-vertices iz) 0.0))
    line-vertices))


;;


(defun half (n) (/ n 2))
