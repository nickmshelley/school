#lang racket
(require (for-syntax syntax/parse)
         (for-syntax racket/list)
         "semantics.rkt"
         "render.rkt")

(provide #%datum #%top #%app #%top-interaction first
         l-system
         (rename-out [my-module-begin #%module-begin]))

(define-syntax (l-system stx)
  (syntax-parse 
   stx
   [(l-system 
     (~datum beta) (~datum ->) beta*:number
     (~datum start-heading) (~datum ->) start-heading*:expr
     (~datum start-up) (~datum ->) start-up*:expr
     (~datum radius) (~datum ->) radius*:number
     (~datum length) (~datum ->) length*:number
     (~datum generations) (~datum ->) generations*:number
     (~datum axiom) (~datum ->) (~seq axiom*:id (~peek-not (~datum ->))) ...
     (~seq lhs (~datum ->) (~seq prob:number (~datum =>) 
                                 (~seq prod (~peek-not (~or (~datum ->) (~datum =>))))
                                 ...)
           ...)
     ...)
    (syntax/loc stx
      (render (turtle-eval global-interp
                           (state (turtle (list 0 0 0) 
                                          (make-orientation start-heading* start-up*)
                                          (list (vector 0 0 0))
                                          (vector .2 .1 .2))
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
                            generations* '(axiom* ...)))
              radius*))]))

(define-syntax-rule (my-module-begin body ...)
  (#%plain-module-begin
   (l-system body ...)))

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

(define (normalize l)
  (define len (sqrt (apply + (map * l l))))
  (map (lambda (x) (/ x len)) l))

(define (cross-product l1 l2)
  (define-values (x1 y1 z1) (values (first l1) (second l1) (third l1)))
  (define-values (x2 y2 z2) (values (first l2) (second l2) (third l2)))
  (list (- (* y1 z2) (* z1 y2))
        (- (* z1 x2) (* x1 z2))
        (- (* x1 y2) (* y1 x2))))

(define (make-orientation* h u)
  (define h-norm (normalize h))
  (define u-norm (normalize u))
  (list h-norm 
        (cross-product h-norm u-norm)
        u-norm))

(define-syntax (make-orientation stx)
  (syntax-parse
   stx
   [(make-orientation (x1:number y1:number z1:number)
                      (x2:number y2:number z2:number))
    (syntax/loc stx
      (make-orientation* (list x1 y1 z1) (list x2 y2 z2)))]))