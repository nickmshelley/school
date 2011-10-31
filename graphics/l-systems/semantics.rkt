#lang racket
(require sgl
         sgl/gl-vectors)
(provide (all-defined-out))

; angle is in radians
(struct state (x y z angle) #:mutable #:transparent)

(define (new-state)
  (state 0 0 0 (/ pi 4)))

(define (move-forward a-state)
  (define d 1)
  (set-state-x! a-state (+ (state-x a-state)
                           (* d (cos (state-angle a-state)))))
  (set-state-y! a-state (+ (state-y a-state)
                           (* d (sin (state-angle a-state))))))

(define (vector-from-state a-state)
  (vector (state-x a-state)
          (state-y a-state)
          (state-z a-state)))

(define (move-forward-and-draw a-state)
  ;move to next position
  (move-forward a-state)
  ;output new vertex for line
  (vector-from-state a-state))

(define (rotate-left a-state)
  (define beta (/ pi 4))
  (set-state-angle! a-state (+ (state-angle a-state) beta)))

(define (rotate-right a-state)
  (define beta (/ pi 4))
  (set-state-angle! a-state (- (state-angle a-state) beta)))