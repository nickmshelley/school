#lang racket
(require "semantics.rkt"
         "render.rkt")

(provide l-system
         (rename-out [my-module-begin #%module-begin]))

(define-syntax-rule (my-module-begin body ...)
  (#%plain-module-begin
   body ...))

(define-syntax-rule (productions prod-list)
  (match-lambda
    (for ([prod prod-list]) ;this needs to be a macro for that goes away
      [(first prod) (second prod)])
    [x (list (list 1 (list x)))]))

(define-syntax-rule (l-system beta start-angle length generations axiom productions)
  (render (turtle-eval global-interp
                                 (state (turtle 0 0 (* start-angle 0.0174532925) (list (vector 0 0 0)) (vector .3 .1 .3))
                                        empty
                                        empty
                                        empty
                                        length
                                        beta)
                                 (eval-lsys (productions) generations axiom))))

