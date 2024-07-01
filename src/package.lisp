(defpackage #:feedback
  (:use #:cl)
  (:nicknames :fb)
  (:export :app

           ;; shaders
           :base-vertex-shader
           :base-fragment-shader
           :*default-shader-program*
           :initialize-default-shader-program

           ;;gl utils
           :gen-gl-array-buffer

           ;;geometry
           :triangle-points

           ;;math
           :vec-mul-scalar))
