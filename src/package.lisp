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
           :vecs/
           ;interpolation
           :bezier-lerp
           :bezier-lerp*))

(defpackage #:dripsy
  (:use #:cl #:arrows)
  (:nicknames :drip)
  (:import-from :dripsy.math :vecs*
                             :bezier-lerp)
  (:export :make
           :width
           :height
           :frame-num

           :on-key-pressed
           :on-resized

           ;primitives
           :point
           :point*
           :line
           :line*
           :eq-tri
           :eq-tri*
           :tri
           :tri*
           :rect
           :rect*
           :polygon
           :polygon*
           :circle
           :circle*
           :bezier
           :bezier*

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

           :noise
           :noise-seed
           :noise-detail))
