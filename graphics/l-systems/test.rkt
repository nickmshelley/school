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

