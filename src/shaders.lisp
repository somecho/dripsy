(in-package #:dripsy)


(defvar base-vertex-shader "
#version 330
layout (location = 0) in vec3 aPos;
layout (location = 1) in vec4 aCol;
uniform mat4 modelViewProjectionMatrix;
out vec4 vCol;
void main(){
  gl_Position = vec4(aPos, 1.0) * modelViewProjectionMatrix;
  vCol = aCol;
}")

(defvar base-fragment-shader "
#version 330
in vec4 vCol;
out vec4 FragColor;

void main()
{
    FragColor = vCol;
}")


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
