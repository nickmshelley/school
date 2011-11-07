#lang racket
(require (for-syntax syntax/parse
                     racket/syntax)
         racket/stxparam
         racket/splicing
         "semantics.rkt"
         "render.rkt")

(provide #%datum #%top #%app
         (rename-out [my-module-begin #%module-begin]))

(define-syntax (my-module-begin stx)
  (syntax-parse 
   stx
   [(_ interp:expr prod:expr ...)
    (with-syntax ([these-productions (generate-temporary)])
      (syntax/loc stx
        (#%plain-module-begin
         (define-productions these-productions
            prod ...)
         (define-interp these-productions interp))))]))


(define-syntax (define-interp stx)
  (syntax-parse
   stx
   [(_ prods (mode expr ... start))
    (syntax/loc stx
      ((mode expr ... 'start) prods))]))

(define-syntax (define-productions stx)
  (syntax-parse
   stx
   [(_ prods:id
       (top:id (~datum ->) 
               (~seq prob:number (~datum =>) 
                     (~and (~not (~datum =>)) 
                           output:expr)
                     ...)
               ...)
       ...)
    (syntax/loc stx
      (define prods
        (match-lambda
          ['top (list (list prob (kinda-quote-list output ...))
                      ...)]
          ...
          [x (list (list 1 (list x)))])))]))

(define-syntax (kinda-quote-list stx)
  (syntax-parse
   stx
   [(_)
    (syntax/loc stx
      empty)]
   [(_ x:id)
    (syntax/loc stx 
      (list 'x))]
   [(_ (i:expr ...))
    (syntax/loc stx
      (list* '\[
             (append 
              (kinda-quote-list i ...)
              (list '\]))))]
   [(_ fst ...)
    (syntax/loc stx
      (append (kinda-quote-list fst)
              ...))]))

(provide 2d)
(define ((2d #:beta beta
             #:start-angle start-angle
             #:length length
             #:generations [generations 5]
             axiom)
         productions)
  (render (turtle-eval global-interp
                       (state (turtle 0 0 (* start-angle 0.0174532925) (list (vector 0 0 0)) (vector .3 .1 .3))
                              empty
                              empty
                              empty
                              length
                              beta)
                       (eval-lsys productions generations axiom))))

