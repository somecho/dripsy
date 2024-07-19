;;;; TRANSFORMATIONS

(in-package #:dripsy)



(defun translate (x y &optional (z 0.0))
  "Translates all subsequent draw calls by x,y and optionally z. Translation is
cumulative. Be sure to push/pop or reset the matrix."
  (let* ((x (coerce x 'single-float))
         (y (coerce y 'single-float))
         (z (coerce z 'single-float))
         (transl-mat (kit.glm:translate* x y z)))
    (with-accessors ((mvp-matrix mvp-matrix))
        *renderer*
      (setf mvp-matrix (kit.glm:matrix* mvp-matrix transl-mat)))))



(defun rotate (theta)
  "Rotates all subsequent draw calls by THETA around the Z-axis. Rotation is
cumulative. Be sure to push/pop or reset the matrix."
  (let* ((theta (coerce theta 'single-float))
         (transl-mat (kit.glm:rotate* 0.0 0.0 theta)))
    (with-accessors ((mvp-matrix mvp-matrix))
        *renderer*
      (setf mvp-matrix (kit.glm:matrix* mvp-matrix transl-mat)))))



(defun push-matrix ()
  "Pushes the current MVP matrix onto an internal stack."
  (with-accessors ((mvp-matrix mvp-matrix)
                   (matrix-stack matrix-stack))
      *renderer*
    (setf matrix-stack (cons matrix-stack mvp-matrix))))



(defun pop-matrix ()
  "Sets the last MVP matrix in the matrix stack as the current MVP matrix and
removes it from the stack."
  (with-accessors ((mvp-matrix mvp-matrix)
                   (matrix-stack matrix-stack))
      *renderer*
    (let ((last-mat (cdr matrix-stack)))
      (setf mvp-matrix last-mat)
      (setf matrix-stack (car matrix-stack)))))



(defun reset-matrix ()
  "Empties the matrix stack and reverts the MVP matrix to the default matrix."
  (with-accessors ((model-matrix model-matrix)
                   (view-matrix view-matrix)
                   (projection-matrix projection-matrix)
                   (mvp-matrix mvp-matrix)
                   (matrix-stack matrix-stack))
      *renderer*
    (setf matrix-stack nil)
    (setf mvp-matrix (kit.glm:matrix* model-matrix view-matrix projection-matrix))))
