;; HELLO TRIANGLE!
;; This example uses the Feedback api and some GL calls to draw a triangle on
;; the screen. It is usually the first example you do when learning openGL.

(drip:make hello-triangle
           (gl:clear :color-buffer)
           (gl:draw-arrays :triangles 0 3))

(defmethod drip:setup ((app hello-triangle))
  (let ((*vbo* nil)
        (*vao* nil))
    (setf *vbo* (gl:gen-buffer))
    (gl:bind-buffer :array-buffer *vbo*)
    (let ((array (drip:gen-gl-array-buffer (drip:triangle-points))))
      (gl:buffer-data :array-buffer :static-draw array))
    (setf *vao* (gl:gen-vertex-array))
    (gl:bind-vertex-array *vao*)
    (gl:enable-vertex-attrib-array 0)
    (gl:vertex-attrib-pointer 0 3 :float :false 0 0)
    (gl:use-program drip:*default-shader-program*)))

(make-instance 'hello-triangle :width 512
                               :height 512)
