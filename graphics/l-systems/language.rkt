#lang racket
(require (for-syntax syntax/parse)
         "semantics.rkt"
         "render.rkt")

(provide #%datum #%top #%app #%top-interaction
         l-system
         (rename-out [my-module-begin #%module-begin]))

(define-syntax-rule (my-module-begin body ...)
  (#%plain-module-begin
   (l-system body ...)))

(define-syntax-rule (productions (lhs (prob prod ...) ...) ...)
  (match-lambda
    ['lhs '((prob (prod ...))
            ...)]
    ...
    [x (list (list 1 (list x)))]))

(define-syntax (l-system stx)
  (syntax-parse stx
                [(l-system 
                  (~datum beta) (~datum ->) beta*:number
                  (~datum start-angle) (~datum ->) start-angle*:number 
                  (~datum length) (~datum ->) length*:number
                  (~datum generations) (~datum ->) generations*:number
                  (~datum axiom) (~datum ->) (~seq axiom*:id (~peek-not (~datum ->))) ...
                  (~seq lhs (~datum ->) (~seq prob:number (~datum =>) 
                                              (~seq prod:id (~peek-not (~datum ->)))
                                              ...)
                        ...)
                  ...)
                 #'(render (turtle-eval global-interp
                                        (state (turtle 0 0 (* start-angle* 0.0174532925) (list (vector 0 0 0)) (vector .3 .1 .3))
                                               empty
                                               empty
                                               empty
                                               length*
                                               beta*)
                                        (eval-lsys 
                                         (match-lambda
                                           ['lhs '((prob (prod ...))
                                                   ...)]
                                           ...
                                           [x (list (list 1 (list x)))])
                                         generations* '(axiom* ...))))]))

