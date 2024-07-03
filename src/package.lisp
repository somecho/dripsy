(defpackage #:dripsy
  (:use #:cl)
  (:nicknames :drip)
  (:export ;; app
           :make
           :setup
           :on-key-pressed
           :on-resized
           :width
           :height
           :frame-num

           ;; renderer
           :renderer
           :*renderer*
           :triangle

           ;; shaders
           :base-vertex-shader
           :base-fragment-shader
           :*default-shader-program*
           :initialize-default-shader-program
           :create-default-shader-program

           ;;gl utils
           :gen-gl-array-buffer

           ;;geometry
           :triangle-points

           ;;math
           :vec-mul-scalar))
