(asdf:defsystem #:dripsy
  :description "Creative Coding Framework"
  :author "Somē Cho <itssomicho@gmail.com>"
  :license  "MIT"
  :version "0.0.14"
  ;; :serial t
  :depends-on (#:alexandria
               #:cl-opengl
               #:cl-glfw3
               #:glkit
               #:arrows
               #:zpng)
  :pathname "src"
  :components ((:file "package")
               (:file "math")
               (:file "dripsy")
               (:file "geom")
               (:file "gl")
               (:file "renderer" :depends-on ("color"))
               (:file "shaders")
               (:file "color")))
