;;;; VEC

(in-package #:dripsy.math)

;;; VECTOR CREATION



(defun vec2 (a b)
  "Creates a simple vector with 2 components."
  `#(,a ,b))



(defun vec2* (a)
  "Creates a simple vector with 2 components, both being A."
  (vec2 a a))



(defun vec3 (a b c)
  "Creates a simple vector with 3 components."
  `#(,a ,b ,c))



(defun vec3* (a)
  "Creates a simple vector with 3 components, all being A."
  (vec3 a a a))



(defun vec4 (a b c d)
  "Creates a simple vector with 4 components."
  `#(,a ,b ,c ,d))



(defun vec4* (a)
  "Creates a simple vector with 4 components, all being A."
  (vec4 a a a a))


;;; VECTOR-VECTOR OPERATIONS



(defun vec+ (&rest vectors)
  "Sums 2 or more simple vectors together. The vectors must be of the same
dimension or the result will have the dimension of the shortest vector."
  (apply #'map 'vector #'+ vectors))



(defun vec- (&rest vectors)
  "Subtracts 2 or more simple vectors together. The vectors must be of the same
dimension or the result will have the dimension of the shortest vector."
  (apply #'map 'vector #'- vectors))



(defun vec* (&rest vectors)
  "Multiplies 2 or more simple vectors together. The vectors must be of the same
dimension or the result will have the dimension of the shortest vector."
  (apply #'map 'vector #'* vectors))



(defun vec/ (&rest vectors)
  "Divides 2 or more simple vectors together. The vectors must be of the same
dimension or the result will have the dimension of the shortest vector."
  (apply #'map 'vector #'/ vectors))



;;; VECTOR-SCALAR OPERATIONS



(defun vecs+ (v s)
  "Adds S to all components of V and return the result as a simple vector."
  (map 'vector (lambda (x) (+ x s)) v))



(defun vecs- (v s)
  "Subtracts S from all components of V and return the result as a simple
vector."
  (map 'vector (lambda (x) (- x s)) v))



(defun vecs* (v s)
  "Multiplies S with all components of V and return the result as a simple
vector."
  (map 'vector (lambda (x) (* x s)) v))



(defun vecs/ (v s)
  "Divides all components of V by S and return the result as a simple vector."
  (map 'vector (lambda (x) (/ x s)) v))
