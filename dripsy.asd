(asdf:defsystem #:dripsy
  :description "Creative Coding Framework"
  :author "Somē Cho <itssomicho@gmail.com>"
  :license  "MIT"
  :version "0.1.0"
  :depends-on (#:alexandria
               #:cl-opengl
               #:cl-glfw3
               #:glkit
               #:arrows
               #:zpng
               #:random-state)
  :pathname "src"
  :components ((:file "package")
               (:file "color")
               (:file "dripsy")
               (:file "geom" :depends-on ("math"))
               (:file "gl" :depends-on ("renderer"))
               (:file "math")
               (:file "math/vec")
               (:file "math/interpolation" :depends-on ("math/vec"))
               (:file "noise")
               (:file "primitives"
                :depends-on ("renderer" "math" "math/interpolation"))
               (:file "renderer" :depends-on ("color"))
               (:file "shaders")
               (:file "transformations" :depends-on ("renderer"))))
