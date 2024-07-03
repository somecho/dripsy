(in-package #:dripsy)


(defun u8p (n)
  (let ((num (coerce n 'single-float)))
    (and (<= num 255.0) (>= num 0.0))))


(deftype u8 ()
  "An integer or and f32 between 0 and 255"
  `(and gl-num (satisfies u8p)))
