#lang racket
(provide (all-defined-out))

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

; p is a list of x, y, z
; orientation is a matrix (list of lists) of H, L, U
; H, L, and U are lists of x, y, z (representing orientation vectors heading, left, and up)
(struct turtle (p orientation verts color))
(struct state (turt turtle-stack branch-verts colors d beta))

(define global-interp
  (match-lambda
    ['F move-forward-and-draw]
    ['\[ push-turtle]
    ['\] pop-turtle]
    ['- rotate-right]
    ['+ rotate-left]
    ['& pitch-down]
    ['^ pitch-up]
    ['> roll-right]
    ['< roll-left]
    ['! turn-around]
    [x (lambda (x) x)]))

(define (find-production prob roll prods)
  (match prods
    [(list (list num ans)) 
     ans]
    [(list (list num ans) another ...)
     (if (< roll (+ prob num))
         ans
         (find-production (+ prob num) roll another))]))

(define (random-production prods)
  (define roll (random))
  (find-production 0 roll prods))

(define (perform-move p v)
  (map + p v))

(define (move-forward a-state)
  (define d (state-d a-state))
  (define the-turtle (state-turt a-state))
  (define heading (first (turtle-orientation the-turtle)))
  (define scaled
    (map (lambda (x)
           (* x d))
         (first (turtle-orientation the-turtle))))
  (struct-copy state a-state
               [turt (struct-copy turtle the-turtle
                                  [p (perform-move
                                      (turtle-p the-turtle)
                                      scaled)])]))

(define (Ru alpha)
  (define delta (* alpha 0.0174532925))
  (define s (sin delta))
  (define c (cos delta))
  (list (list c s 0)
        (list (- s) c 0)
        (list 0 0 1)))

(define (Rl alpha)
  (define delta (* alpha 0.0174532925))
  (define s (sin delta))
  (define c (cos delta))
  (list (list c 0 (- s))
        (list 0 1 0)
        (list s 0 c)))

(define (Rh alpha)
  (define delta (* alpha 0.0174532925))
  (define s (sin delta))
  (define c (cos delta))
  (list (list 1 0 0)
        (list 0 c (- s))
        (list 0 s c)))

(define (transpose mat)
  (apply map list mat))

(define (m-mult-helper m1 m2)
  (for/list ([r m1]) 
    (for/list ([c (transpose m2)])
      (apply + (map * r c)))))

(define (m-mult m1 m2)
  (transpose (m-mult-helper (transpose m1) m2)))

(define (update-orientation a-turtle mat)
  (define orientation (turtle-orientation a-turtle))
  (struct-copy turtle a-turtle
               [orientation (m-mult orientation mat)]))

(define (rotate-left a-state)
  (define beta (state-beta a-state))
  (struct-copy state a-state
               [turt (update-orientation (state-turt a-state) (Ru beta))]))

(define (rotate-right a-state)
  (define beta (state-beta a-state))
  (struct-copy state a-state
               [turt (update-orientation (state-turt a-state) (Ru (- beta)))]))

(define (pitch-up a-state)
  (define beta (state-beta a-state))
  (struct-copy state a-state
               [turt (update-orientation (state-turt a-state) (Rl beta))]))

(define (pitch-down a-state)
  (define beta (state-beta a-state))
  (struct-copy state a-state
               [turt (update-orientation (state-turt a-state) (Rl (- beta)))]))

(define (roll-left a-state)
  (define beta (state-beta a-state))
  (struct-copy state a-state
               [turt (update-orientation (state-turt a-state) (Rh beta))]))

(define (roll-right a-state)
  (define beta (state-beta a-state))
  (struct-copy state a-state
               [turt (update-orientation (state-turt a-state) (Rh (- beta)))]))

(define (turn-around a-state)
  (struct-copy state a-state
               [turt (update-orientation (state-turt a-state) (Ru 180))]))

(define (vector-from-state a-state)
  (define p (turtle-p (state-turt a-state)))
  (list->vector p))

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