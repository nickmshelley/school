#lang racket
(require sgl
         sgl/gl-vectors)
(provide get-vector-elements
         normalize
         sub-vectors
         cross-prod
         add-vectors
         point-up
         normal-from-vectors)

(define (get-vector-elements vec)
  (values (gl-vector-ref vec 0)
          (gl-vector-ref vec 1)
          (gl-vector-ref vec 2)))

(define (normalize vec)
  (define-values (x y z) (get-vector-elements vec))
  (define sum-length (sqrt (+ (expt x 2) (expt y 2) (expt z 2))))
  (gl-float-vector (/ x sum-length)
                   (/ y sum-length)
                   (/ z sum-length)))

(define (sub-vectors v1 v2)
  (define-values (ax ay az) (get-vector-elements v1))
  (define-values (bx by bz) (get-vector-elements v2))
  (gl-float-vector (- ax bx)
                   (- ay by)
                   (- az bz)))

;cross-prod : gl-vector gl-vector -> gl-float-vector
(define (cross-prod vec1 vec2)
  (define-values (x1 y1 z1) (get-vector-elements vec1))
  (define-values (x2 y2 z2) (get-vector-elements vec2))
  (gl-float-vector (- (* y1 z2) (* z1 y2))
                   (- (* z1 x2) (* x1 z2))
                   (- (* x1 y2) (* y1 x2))))

;add-vectors : listof gl-vector -> gl-vector
(define (add-vectors vec-list)
  (list->gl-float-vector
   (apply map + (map gl-vector->list vec-list))))

(define (point-up vec)
  (define-values (x y z) (get-vector-elements vec))
  (if (< y 0)
      (gl-float-vector (- x) (- y) (- z))
      vec))

(define (normal-from-vectors v1 v2 v3)
  (if (and v1 v2 v3)
      (cross-prod (sub-vectors v1 v2) 
                  (sub-vectors v2 v3))
      (gl-float-vector 0 0 0)))