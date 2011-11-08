#lang racket
(provide (all-defined-out))

(struct turtle (x y angle verts color))
(struct state (turt turtle-stack branch-verts colors d beta))

(define (find-production prob roll prods)
  (match prods
    [(list (list num ans)) 
     ans]
    [(list (list num ans) another ...)
     (if (< roll (+ prob num))
         ans
         (find-production (+ prob num) roll another))]))

;ask how to do this cool
(define (random-production prods)
  (define roll (random))
  (find-production 0 roll prods))

;eval-lsys : (symbol->listOfSymbol) nat listOfSymbol -> listOfSymbol
(define (eval-lsys produce generations axiom)
  (if (zero? generations)
      axiom
      (eval-lsys produce (sub1 generations) (append-map random-production (map produce axiom)))))

;turtle-eval : (symbol->function) state listOfSymbol -> state
(define (turtle-eval interps state string)
  (if (empty? string)
      state
      (turtle-eval interps
                   ((interps (first string)) state)
                   (rest string))))

(define (move-forward a-state)
  (define d (state-d a-state))
  (define the-turtle (state-turt a-state))
  (struct-copy state a-state
               [turt (struct-copy turtle the-turtle
                                  [x (+ (turtle-x the-turtle)
                                        (* d (cos (turtle-angle the-turtle))))]
                                  [y (+ (turtle-y the-turtle)
                                        (* d (sin (turtle-angle the-turtle))))])]))

(define (add-angle a-state angle)
  (define the-turtle (state-turt a-state))
  (struct-copy state a-state
               [turt (struct-copy turtle the-turtle
                                  [angle (+ (turtle-angle the-turtle) angle)])]))

(define (rotate-left a-state)
  (define beta (* (state-beta a-state) 0.0174532925))
  (add-angle a-state beta))

(define (rotate-right a-state)
  (define beta (* (state-beta a-state) 0.0174532925))
  (add-angle a-state (- beta)))

(define (vector-from-state a-state)
  (define the-turtle (state-turt a-state))
  (vector (turtle-x the-turtle)
          (turtle-y the-turtle)
          0))

(define (add-vert a-state vert)
  (define the-turtle (state-turt a-state))
  (struct-copy state a-state
               [turt (struct-copy turtle the-turtle
                                  [verts (cons vert (turtle-verts the-turtle))])]))

(define (move-forward-and-draw a-state)
  ;move to next position
  (define next-state (move-forward a-state))
  ;output new vertex for line
  (define vec (vector-from-state next-state))
  (add-vert next-state vec))

(define (clear-turtle-verts a-turtle)
  (define color (turtle-color a-turtle))
  (struct-copy turtle a-turtle
               [verts (list (first (turtle-verts a-turtle)))]
               [color (vector (vector-ref color 0)
                              (+ (vector-ref color 1) .1)
                              (vector-ref color 2))]))

(define (push-turtle a-state)
  (struct-copy state a-state
               [turt (clear-turtle-verts (state-turt a-state))]
               [turtle-stack (cons (state-turt a-state) 
                                   (state-turtle-stack a-state))]))

(define (pop-turtle a-state)
  (struct-copy state a-state
               [turt (first (state-turtle-stack a-state))]
               [turtle-stack (rest (state-turtle-stack a-state))]
               [branch-verts (cons (turtle-verts (state-turt a-state))
                                   (state-branch-verts a-state))]
               [colors (cons (turtle-color (state-turt a-state))
                             (state-colors a-state))]))

(define global-interp
  (match-lambda
    ['F move-forward-and-draw]
    ['- rotate-right]
    ['+ rotate-left]
    ['\[ push-turtle]
    ['\] pop-turtle]
    [x (lambda (x) x)]))