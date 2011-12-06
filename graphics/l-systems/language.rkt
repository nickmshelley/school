#lang racket
(require (for-syntax syntax/parse)
         (for-syntax racket/list)
         "semantics.rkt"
         "render.rkt")

(provide #%datum #%top #%app #%top-interaction first
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

(define-syntax (kinda-quote stx)
  (syntax-parse
   stx
   [(_ x:id) (syntax (list 'x))]
   [(_ (x ...)) (syntax
                (append (list '\[)
                        (kinda-quote x ...)
                        (list '\])))]
   [(_ fst ...) (syntax (append
                         (kinda-quote fst) ...))]))

(define-for-syntax (normalize l)
  (define len (sqrt (apply + (map * l l))))
  (map (lambda (x) (/ x len)) l))

(define-for-syntax (cross-product l1 l2)
  (define-values (x1 y1 z1) (values (first l1) (second l1) (third l1)))
  (define-values (x2 y2 z2) (values (first l2) (second l2) (third l2)))
  (list (- (* y1 z2) (* z1 y2))
                   (- (* z1 x2) (* x1 z2))
                   (- (* x1 y2) (* y1 x2))))

(define-syntax (make-orientation h u)
  (define h-norm (normalize h))
  (define u-norm (normalize u))
  (define l (cross-product h-norm u-norm))
  #'(list h-norm u-norm l))

(define-syntax (l-system stx)
  (syntax-parse 
   stx
   [(l-system 
     (~datum beta) (~datum ->) beta*:number
     (~datum start-heading) (~datum ->) ((~seq start-heading*:number ...))
     (~datum start-up) (~datum ->) ((~seq start-up*:number ...))
     (~datum length) (~datum ->) length*:number
     (~datum generations) (~datum ->) generations*:number
     (~datum axiom) (~datum ->) (~seq axiom*:id (~peek-not (~datum ->))) ...
     (~seq lhs (~datum ->) (~seq prob:number (~datum =>) 
                                 (~seq prod (~peek-not (~or (~datum ->) (~datum =>))))
                                 ...)
           ...)
     ...)
    #'(render (turtle-eval global-interp
                           (state (turtle (list 0 0 0) 
                                          (list (list 0 1 0)
                                                (list -1 0 0)
                                                (list 0 0 1))
                                          ;(make-orientation (list start-heading* ...) (list start-up* ...))
                                          (list (vector 0 0 0))
                                          (vector .3 .1 .3))
                                  empty
                                  empty
                                  empty
                                  length*
                                  beta*)
                           (eval-lsys 
                            (match-lambda
                              ['lhs `((prob ,(kinda-quote prod ...))
                                      ...)]
                              ...
                              [x (list (list 1 (list x)))])
                            generations* '(axiom* ...))))]))
    
    
    