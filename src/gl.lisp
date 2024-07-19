;;;; GL
;;;; Some what more conveniently wrapped GL calls

(in-package #:dripsy)


(defun gen-gl-array-buffer (data &key (type :float))
  (let ((arr (gl:alloc-gl-array type (length data))))
    (dotimes (i (length data))
      (setf (gl:glaref arr i) (aref data i)))
    arr))



;;; RENRDERER METHODS



(defmethod bind-array-buffer ((renderer renderer))
  "Binds the renderer's VBO if it isn't already bound."
  (with-accessors ((vertex-buffer-location vertex-buffer-location))
      renderer
    (let ((current-array-buffer (gl:get-integer :array-buffer-binding) ))
      (unless (eq vertex-buffer-location current-array-buffer)
        (gl:bind-buffer :array-buffer vertex-buffer-location)))))



(defmethod bind-vertex-array ((renderer renderer))
  "Binds the renderer's VAO if it isn't already bound."
  (with-accessors ((vertex-attrib-location vertex-attrib-location))
      renderer
    (let ((current-attrib (gl:get-integer :vertex-array-binding) ))
      (unless (eq vertex-attrib-location current-attrib)
        (gl:bind-vertex-array vertex-attrib-location)))))



(defmethod write-array-buffer ((renderer renderer) data)
  "Uploads the vertex attributes position and color to OpenGL if the vertex
position data has changed."
  (with-accessors ((vertices vertices)
                   (col col)
                   (colors colors))
      renderer
    (unless (equalp data vertices)
      (setf vertices data)
      (setf colors (flat-repeat col (array-total-size data)))
      (let ((vertex-data (interlace-arrays vertices colors 3 4)))
        (gl:buffer-data :array-buffer :dynamic-draw
                        (gen-gl-array-buffer vertex-data))))))



(defmethod write-index-buffer ((renderer renderer) data)
  (with-accessors ((indices indices)
                   (index-buffer-location index-buffer-location))
      renderer
    (unless (equalp data indices)
      (setf indices data)
      (gl:bind-buffer :element-array-buffer index-buffer-location)
      (gl:buffer-data :element-array-buffer :dynamic-draw
                      (gen-gl-array-buffer data :type :unsigned-int)))))
