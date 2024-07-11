(defpackage #:dripsy
  (:use #:cl #:arrows)
  (:nicknames :drip)
  (:export ;; public app vars
           :make
           :width
           :height
           :frame-num

           ;; app callbacks
           :on-key-pressed
           :on-resized

           ;; public draw API
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

           :set-color
           :set-circle-resolution
           :use-fill!
           :no-fill!

           :half))
