(defpackage #:dripsy
  (:use #:cl #:arrows)
  (:nicknames :drip)
  (:export :make
           :width
           :height
           :frame-num

           :on-key-pressed
           :on-resized

           :point
           :points
           :line
           :eq-tri
           :tri
           :rect
           :polygon
           :circle
           :save-screen
           :clear
           :background
           :pop-matrix
           :push-matrix
           :reset-matrix
           :translate
           :rotate

           :set-color
           :set-circle-resolution
           :use-fill!
           :no-fill!

           :half
           :noise
           :noise-seed
           :noise-detail))
