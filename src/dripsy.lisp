(in-package #:dripsy)


;; DRIPSY BASE APPLICATION CLASS


(defclass base-app ()
  ((width :initform 1024
          :initarg :width
          :accessor width)
   (height :initform 768
           :initarg :height
           :accessor height)
   (title :initform "Dripsy App"
          :initarg :title
          :accessor title)
   (gl-version-major :initform 3
                     :initarg :gl-version-major
                     :accessor gl-version-major)
   (gl-version-minor :initform 3
                     :initarg :gl-version-minor
                     :accessor gl-version-minor)
   (initialized :initform nil
                :accessor initialized)
   (frame-num :initform 0
              :accessor frame-num)
   (default-shader :initform nil
                   :accessor default-shader)))


;; GENERICS


(defgeneric setup (dripsy-app)
  (:documentation "This is called once at the beginning before the draw loop
  begins. To access properties like width, you will need to use an accessor
  function."))


(defgeneric on-key-pressed (dripsy-app key)
  (:documentation "The callback for the dripsy app that is called when a key is
  pressed."))


(defgeneric on-resized (dripsy-app width height)
  (:documentation "The callback for the dripsy app that is called when the
  window is resized."))


;; DEFAULT DEFMETHODS


(defun make-setup (app-name)
  `(defmethod setup ((,app-name ,app-name))))


(defun make-define-on-key-pressed (app-name)
  `(defmethod on-key-pressed ((,app-name ,app-name) key)))


(defun make-define-on-resized (app-name)
  `(defmethod on-resized ((,app-name ,app-name) width height)))


;; DRIPSY APP


(defun parse-blocks (body)
  (let ((setup nil)
        (draw nil)
        (current-block :draw)
        (blocks nil))
    (setf blocks
          (loop for exp in body
                do (cond ((eq exp :setup) (setf current-block :setup))
                         ((eq exp :draw) (setf current-block :draw)))
                if (and (eq current-block :setup) (not (keywordp exp)))
                  collect exp into setup
                if (and (eq current-block :draw) (not (keywordp exp)))
                  collect exp into draw
                finally (return (list :setup setup
                                      :draw draw))))
    `(,@blocks)))


(defun make-define-app-class (app-name)
  `(defclass ,app-name (base-app) ()))


(defun make-define-initialize-class (app-name body)
  (let ((blocks (parse-blocks body)))
    `(defmethod initialize-instance :after ((,app-name ,app-name) &key)
       (with-accessors ((width width)
                        (height height)
                        (title title)
                        (gl-version-major gl-version-major)
                        (gl-version-minor gl-version-minor)
                        (initialized initialized)
                        (frame-num frame-num)
                        (default-shader default-shader))
           ,app-name
         (unless initialized
           (%glfw:init)
           (%glfw:window-hint :context-version-major gl-version-major)
           (%glfw:window-hint :context-version-minor gl-version-minor)
           (let ((window (%glfw:create-window width height title (cffi:null-pointer) (cffi:null-pointer))))
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
                (declare (ignore scancode mods))
                (when (eq action :press)
                  (on-key-pressed ,app-name key)
                  (when (eq key :escape)
                    (glfw:set-window-should-close window)))))
             (glfw:set-window-size-callback
              (cffi:defcallback resize-callback :void ((window :pointer)
                                                       (w :int)
                                                       (h :int))
                (declare (ignore window))
                (on-resized ,app-name w h)
                (gl:viewport 0 0 w h)))
             (setf *renderer* (make-instance 'renderer :width width :height height))
             (progn ,@(getf blocks :setup))
             (loop until (glfw:window-should-close-p window)
                   do (incf frame-num)
                   do (progn ,@(getf blocks :draw))
                   do (glfw:swap-buffers window)
                   do (glfw:poll-events))
             (glfw:destroy-window window)
             (glfw:terminate)))
         (setf initialized t)))))


(defmacro make (app-name &rest body)
  `(progn ,(make-define-app-class app-name)
          ,(make-setup app-name)
          ,(make-define-on-key-pressed app-name)
          ,(make-define-on-resized app-name)
          ,(make-define-initialize-class app-name body)))
