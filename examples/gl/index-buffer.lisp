;; INDEX BUFFER
;; This example uses GL calls to render a quad using an index buffer.

(defvar quad-vertices
  (drip:gen-gl-array-buffer #(400.0 400.0 0.0
                              400.0 200.0 0.0
                              200.0 200.0 0.0
                              200.0 400.0 0.0)))

(defvar quad-indices (drip:gen-gl-array-buffer #(0 1 3 1 2 3)
                                               :type :unsigned-int))

(drip:make quad
           :setup
           (let ((ebo (gl:gen-buffer))
                 (vbo (gl:gen-buffer))
                 (vao (gl:gen-vertex-array)))
             (gl:bind-buffer :array-buffer vbo)
             (gl:buffer-data :array-buffer :static-draw quad-vertices)
             (gl:bind-buffer :element-array-buffer ebo)
             (gl:buffer-data :element-array-buffer :static-draw quad-indices)
             (gl:bind-vertex-array vao)
             (gl:enable-vertex-attrib-array 0)
             (gl:vertex-attrib-pointer 0 3 :float :false 0 0))
           :draw
           (gl:draw-elements :triangles quad-indices))

(make-instance 'quad :width 500
                     :height 500)
