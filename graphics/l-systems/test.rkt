#lang racket
(require rackunit)


;table lookup from char to string
(define (algae c)
  (match c
    ['a '(a b)]
    ['b '(a)]
    [x (list x)]))

(check-equal? (algae 'a) '(a b))
(check-equal? (algae 'b) '(a))
(check-equal? (algae 'c) '(c))

;eval-lsys : (symbol->listOfSymbol) nat listOfSymbol -> listOfSymbol
(define (eval-lsys produce generations axiom)
  (if (zero? generations)
      axiom
      (eval-lsys produce (sub1 generations) (append-map produce axiom))))
(check-equal? (eval-lsys algae 0 '(a)) '(a))
(check-equal? (eval-lsys algae 1 '(a)) '(a b))
(check-equal? (eval-lsys algae 2 '(a)) '(a b a))
(check-equal? (eval-lsys algae 3 '(b)) '(a b a))

;turtle-eval : (symbol->function) state listOfSymbol -> state
(define (turtle-eval interps state string)
  (if (empty? string)
      state
      (turtle-eval interps
                   ((interps (first string)) state)
                   (rest string))))

(define algae-interp
  (match-lambda
    ['a (lambda (x) (print "hi") (add1 x))]
    ['b (lambda (x) (print 2) (add1 x))]
    [x (lambda (x) x)]))

(struct turtle (x y angle))
(struct state (turt verts))

;table lookup from char to string
(define line
  (match-lambda
    ['F '(F F)]
    [x (list x)]))
(check-equal? (eval-lsys line 2 '(F)) '(F F F F))

(define (move-forward a-state)
  (define d 1)
  (define the-turtle (state-turt a-state))
  (state (turtle (+ (turtle-x the-turtle)
                    (* d (cos (turtle-angle the-turtle))))
                 (+ (turtle-y the-turtle)
                    (* d (sin (turtle-angle the-turtle))))
                 (turtle-angle the-turtle))
         (state-verts a-state)))

(define (vector-from-state a-state)
  (define the-turtle (state-turt a-state))
  (vector (turtle-x the-turtle)
          (turtle-y the-turtle)
          0))

(define (move-forward-and-draw a-state)
  ;move to next position
  (define next-state (move-forward a-state))
  ;output new vertex for line
  (define vec (vector-from-state next-state))
  (state (state-turt next-state)
         (cons vec (state-verts next-state))))

(define line-interp
  (match-lambda
    ['F (lambda (state) (move-forward-and-draw state))]
    ['f (lambda (state) (move-forward state))]
    [x (lambda (x) x)]))

(define vecs (reverse (state-verts (turtle-eval line-interp 
                                                (state (turtle 0 0 (/ pi 2)) (list (vector 0 0 0)))
                                                (eval-lsys line 2 '(F))))))
(print vecs)
