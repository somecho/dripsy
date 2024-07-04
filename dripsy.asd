(asdf:defsystem #:dripsy
  :description "Creative Coding Framework"
  :author "Somē Cho <itssomicho@gmail.com>"
  :license  "MIT"
  :version "0.0.6"
  :serial t
  :depends-on (#:alexandria
               #:cl-opengl
               #:cl-glfw3
               #:glkit
               #:arrows)
  :pathname "src"
  :components ((:file "package")
               (:file "dripsy")
               (:file "geom")
               (:file "gl")
               (:file "math")
               (:file "renderer")
               (:file "shaders")
               (:file "color")))
