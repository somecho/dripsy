(defun on-key-pressed (key)
  (when (eq key :escape)
    (glfw:set-window-should-close)))

(defvar fps (/ 1.0 120))

(defvar *vbo* nil)
(defvar *vao* nil)
(defvar *vs* nil)
(defvar *fs* nil)
(defvar *shader* nil)

(defun create-window (&key (width 768) (height 1024))
  (%glfw:init)
  (%glfw:window-hint :context-version-major 3)
  (%glfw:window-hint :context-version-major 3)
  (let ((window (%glfw:create-window width height "GLFW" (cffi:null-pointer) (cffi:null-pointer))))
    (unless window (glfw:terminate))
    (setf glfw:*window* window)
    (glfw:make-context-current window)
    (gl:viewport 0 0 width height)
    (glfw:set-key-callback
     (cffi:defcallback key-callback :void ((window :pointer)
                                           (key %glfw::key)
                                           (scancode :int)
                                           (action %glfw::key-action)
                                           (mods %glfw::mod-keys))
       (declare (ignore window scancode mods))
       (when (eq action :press)
         (on-key-pressed key))))
    (fb:initialize-default-shader-program)

    ;; setup block

    (setf *vbo* (gl:gen-buffer))
    (gl:bind-buffer :array-buffer *vbo*)
    (let ((array (fb:gen-gl-array-buffer #(-0.5 0.5 0.0
                                          0.0 0.0 0.0
                                          0.5 0.5 0.0))))
      (gl:buffer-data :array-buffer :static-draw array))

  (setf *vao* (gl:gen-vertex-array))
  (gl:bind-vertex-array *vao*)
  (gl:enable-vertex-attrib-array 0)
  (gl:vertex-attrib-pointer 0 3 :float :false 0 0)

  (gl:use-program fb:*default-shader-program*)

  (loop until (glfw:window-should-close-p)
        do (gl:clear :color-buffer)
        do (gl:use-program *shader*)
        do (gl:draw-arrays :triangles 0 3)

        do (glfw:swap-buffers)
        do (glfw:poll-events)
        do (sleep fps))
  (glfw:destroy-window)
  (glfw:terminate)))

(create-window :width 1230)

(glfw:terminate)
