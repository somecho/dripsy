(defpackage #:basic-shapes
  (:use :cl :dripsy))

(in-package :basic-shapes)

;;;; Basic Shapes
;;;; This example draws five basic shapes. More can be drawn. Take a look at
;;;; `primitives.lisp` for more information on other shapes that can be drawn.

(make app
      ; set the background color
      (background 180 190 200)

      ; draw a circle
      (set-color 255 200 170)
      (circle (* 0.5 width) (* 0.5 height) 100)

      ; draw a line
      (set-color 0 0 0)
      (line (* 0.3 width) (* 0.8 height)
            (* 0.7 width) (* 0.8 height))

      ; draw an equilateral triangle
      (set-color 180 140 160)
      (eq-tri (* 0.2 width) (* 0.2 height) 80)

      ; draw a rectangle
      (set-color 255 255 255)
      (rect (* 0.1 width) (* 0.86 height) 200 100)

      ; draw a heptagon
      (set-color 100 200 20)
      (polygon (* 0.8 width) (* 0.3 height) 100 7))

(make-instance 'app)
