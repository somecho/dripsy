(in-package #:feedback)

(defclass app ()
  ((width
    :initform 720
    :initarg :width
    :accessor width)
   (height
    :initform 480
    :initarg :height
    :accessor height)
   (title
    :initform "GLFW"
    :initarg :title
    :accessor title)
   (setup
    :initform (lambda () (print "hello"))
    :initarg :setup
    :accessor setup)
   (initialized
    :initform nil
    :accessor initialized)))

(glfw:def-key-callback *key-callback-sym*
    (window key scancode action mod-keys)
  (declare (ignore window scancode mod-keys))
  (when (and (eq key :escape) (eq action :press))
    (glfw:set-window-should-close)))

(defmethod initialize-instance :after ((app app) &key)
  (unless (initialized app)
    (glfw:with-init-window (:width (width app)
                            :height (height app)
                            :title (title app)
                            :context-version-major 3
                            :context-version-minor 3)
      (setf %gl:*gl-get-proc-address* #'glfw:get-proc-address)
      (glfw:set-key-callback '*key-callback-sym*)
      (glfw:set-window-size-callback 'fb:on-window-resize)
      (funcall (setup app))

      (loop until (glfw:window-should-close-p)
            do (glfw:swap-buffers)
            do (glfw:poll-events))    ))
  (setf (initialized app) t))
