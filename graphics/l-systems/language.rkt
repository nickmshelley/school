#lang racket
(require "semantics.rkt")

(provide f
         F
         plus
         minus
         (rename-out [my-module-begin #%module-begin]))

(define current-state (make-parameter (new-state)))

(define-syntax-rule (my-module-begin body ...)
  (#%plain-module-begin
   (parameterize ([current-state (new-state)])
     body ...)))

(define-syntax-rule (f)
  (move-forward (current-state)))

(define-syntax-rule (F)
  (move-forward-and-draw (current-state)))

(define-syntax-rule (plus)
  (rotate-left (current-state)))

(define-syntax-rule (minus)
  (rotate-right (current-state)))

(define-syntax-rule (interp body ...)
  (parameterize ([current-state (new-state)])
    body ...))