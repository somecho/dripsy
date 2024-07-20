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
           :use-fill
           :no-fill

           :vec-mul-scalar
           :half
           :noise
           :noise-seed
           :noise-detail))

(defpackage #:dripsy.math
  (:use #:cl #:arrows)
  (:nicknames :drip.math)
  (:export ;vector creation
           :vec2
           :vec2*
           :vec3
           :vec3*
           :vec4
           :vec4*
           ;vector-vector operations
           :vec+
           :vec-
           :vec*
           :vec/
           ;vector-scalar operations
           :vecs+
           :vecs-
           :vecs*
           :vecs/))
