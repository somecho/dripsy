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
              :accessor frame-num)))


;; GENERICS


(defgeneric setup (dripsy-app)
  (:documentation "This is called once at the beginning before the draw loop
  begins. To access properties like width, you will need to use an accessor
  function.")
  (:method ((dripsy-app base-app))))


(defgeneric draw (dripsy-app))


(defgeneric on-key-pressed (dripsy-app key)
  (:documentation "The callback for the dripsy app that is called when a key is
  pressed.")
  (:method ((dripsy-app base-app) key)))


(defgeneric on-resized (dripsy-app width height)
  (:documentation "The callback for the dripsy app that is called when the
  window is resized.")
  (:method ((dripsy-app base-app) width height)))


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


(defun make-draw-method (app-name body)
  (let ((blocks (parse-blocks body)))
    `(defmethod draw ((,app-name ,app-name))
       (with-accessors ((frame-num frame-num)
                        (width width)
                        (height height))
           ,app-name
         (progn ,@(getf blocks :draw))))))


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
                        (frame-num frame-num))
           ,app-name
         (unless initialized
           (%glfw:init)
           (%glfw:window-hint :context-version-major gl-version-major)
           (%glfw:window-hint :context-version-minor gl-version-minor)
           (%glfw:window-hint :samples 4)
           (let ((window (%glfw:create-window width height title (cffi:null-pointer) (cffi:null-pointer))))
             (unless window (glfw:terminate))
             (setf glfw:*window* window)
             (glfw:make-context-current window)
             (gl:viewport 0 0 width height)
             (gl:enable :multisample)
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
                (gl:viewport 0 0 w h)
                (setf (width *renderer*) w)
                (setf (height *renderer*) h)))
             (setf *renderer* (make-instance 'renderer :width width :height height))
             (progn ,@(getf blocks :setup))
             (loop until (glfw:window-should-close-p window)
                   do (incf frame-num)
                   do (draw ,app-name)
                   do (glfw:swap-buffers window)
                   do (glfw:poll-events))
             (glfw:destroy-window window)
             (glfw:terminate)))
         (setf initialized t)))))


(defmacro make (app-name &rest body)
  "This is the main macro for creating Dripsy applications/sketches. It creates
 a class definition with the name APP-NAME which can be later instanciated with
 make-instance. Calling make-instance on the newly created class will start
 Dripsy.

 After the app-name, you can start to use draw functions that run every frame.
 If you would like to use a setup function that only gets called once, you can
 do it by using the :setup keyword like so:

 (drip:make sketch
            :setup
            (print \"This is run once\")
            :draw
            (drip:point 250 250))

 Note that to begin calling the draw functions again, you will need to add the
 keyword :draw before the draw functions."
  `(progn ,(make-define-app-class app-name)
          ,(make-draw-method app-name body)
          ,(make-define-initialize-class app-name body)))
