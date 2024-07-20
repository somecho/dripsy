(defpackage #:transformations
  (:use :cl :dripsy))

(in-package #:transformations)

;;;; Transformations

(make app
      (background 0 0 0)
      (set-color 255 255 255)
      (no-fill) ; call to draw shapes without fill

      ; Let's first store the current 'state'
      (push-matrix)
      ; The origin (initially at 0,0) is moved to 66% to the right and 50% to
      ; the top.
      (translate (* 0.66 width) (* 0.5 height))
      ; Now the center of the circle is at the origin, which is somewhere in the
      ; middle of the screen.
      (circle 0 0 100)
      ; We remove the transformations with this call, otherwise the
      ; transformations get accumulated and our circle will eventually be off
      ; screen.
      (pop-matrix)

      (use-fill) ; draw shapes with fill again
      (push-matrix)
      (translate (* 0.33 width) (* 0.5 height))
      (let ((theta (* 0.005 frame-num)))
        ; Note that rotation happens around the origin. By drawing the polygon
        ; at the origin, we can rotate it like a record player.
        (rotate theta)
        (polygon 0 0 100 9))
      (pop-matrix)

      (translate (* 0.5 width) (* 0.5 height))
      (set-color 220 220 20)
      (let ((theta (* 0.02 frame-num)))
        (rotate theta)
        (tri 200 100
             100 200
             200 200))
      ; Instead of always calling push and pop, we can also call reset-matrix
      ; after we are done with our transformations to restore the origin and
      ; rotation of our screen.
      (reset-matrix))

(make-instance 'app)
