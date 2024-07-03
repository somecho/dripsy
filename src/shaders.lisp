(in-package #:dripsy)

(defparameter *default-shader-program* nil)

(defvar base-vertex-shader "
#version 330
layout (location = 0) in vec3 aPos;
uniform mat4 modelViewProjectionMatrix;
void main(){
  gl_Position = vec4(aPos, 1.0) * modelViewProjectionMatrix;
}")

(defvar base-fragment-shader "
#version 330
out vec4 FragColor;

void main()
{
    FragColor = vec4(1.0f, 0.5f, 0.2f, 1.0f);
}")

(defun initialize-default-shader-program ()
  "Creates a shader program from base default shaders and binds it to the
variable *default-shader-program*. This shader is used for standard rendering."
  (let ((vs (gl:create-shader :vertex-shader))
        (fs (gl:create-shader :fragment-shader))
        (shader nil))
    (gl:shader-source vs base-vertex-shader)
    (gl:shader-source fs base-fragment-shader)
    (gl:compile-shader vs)
    (gl:compile-shader fs)
    (setf shader (gl:create-program))
    (gl:attach-shader shader vs)
    (gl:attach-shader shader fs)
    (gl:link-program shader)
    (gl:delete-shader vs)
    (gl:delete-shader fs)
    (setf *default-shader-program* shader)
    shader))

(defun create-default-shader-program ()
  "Creates a shader program from base default shaders. This shader is used for
standard rendering."
  (let ((vs (gl:create-shader :vertex-shader))
        (fs (gl:create-shader :fragment-shader))
        (shader nil))
    (gl:shader-source vs base-vertex-shader)
    (gl:shader-source fs base-fragment-shader)
    (gl:compile-shader vs)
    (gl:compile-shader fs)
    (setf shader (gl:create-program))
    (gl:attach-shader shader vs)
    (gl:attach-shader shader fs)
    (gl:link-program shader)
    (gl:delete-shader vs)
    (gl:delete-shader fs)
    shader))
