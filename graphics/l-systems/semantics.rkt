#lang racket
(provide (all-defined-out))

(struct turtle (x y angle verts color))
(struct state (turt turtle-stack branch-verts colors d beta))

(define (find-production prob roll prods)
  (if (or (= (length prods) 1)
          (< roll (+ prob (first (first prods)))))
      (second (first prods))
      (find-production (+ prob (first (first prods))) roll (rest prods))))

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
  (state (turtle (+ (turtle-x the-turtle)
                    (* d (cos (turtle-angle the-turtle))))
                 (+ (turtle-y the-turtle)
                    (* d (sin (turtle-angle the-turtle))))
                 (turtle-angle the-turtle)
                 (turtle-verts the-turtle)
                 (turtle-color the-turtle))
         (state-turtle-stack a-state)
         (state-branch-verts a-state)
         (state-colors a-state)
         (state-d a-state)
         (state-beta a-state)))

(define (add-angle a-state angle)
  (define the-turtle (state-turt a-state))
  (state (turtle (turtle-x the-turtle)
                 (turtle-y the-turtle)
                 (+ (turtle-angle the-turtle) angle)
                 (turtle-verts the-turtle)
                 (turtle-color the-turtle))
         (state-turtle-stack a-state)
         (state-branch-verts a-state)
         (state-colors a-state)
         (state-d a-state)
         (state-beta a-state)))

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
  (state (turtle (turtle-x the-turtle)
                 (turtle-y the-turtle)
                 (turtle-angle the-turtle)
                 (cons vert (turtle-verts the-turtle))
                 (turtle-color the-turtle))
         (state-turtle-stack a-state)
         (state-branch-verts a-state)
         (state-colors a-state)
         (state-d a-state)
         (state-beta a-state)))

(define (move-forward-and-draw a-state)
  ;move to next position
  (define next-state (move-forward a-state))
  ;output new vertex for line
  (define vec (vector-from-state next-state))
  (add-vert next-state vec))

(define (clear-turtle-verts a-turtle)
  (define color (turtle-color a-turtle))
  (turtle (turtle-x a-turtle)
          (turtle-y a-turtle)
          (turtle-angle a-turtle)
          (list (first (turtle-verts a-turtle)))
          (vector (vector-ref color 0)
                  (+ (vector-ref color 1) .1)
                  (vector-ref color 2))))

(define (push-turtle a-state)
  (state (clear-turtle-verts (state-turt a-state))
         (cons (state-turt a-state) (state-turtle-stack a-state))
         (state-branch-verts a-state)
         (state-colors a-state)
         (state-d a-state)
         (state-beta a-state)))

(define (pop-turtle a-state)
  (state (first (state-turtle-stack a-state))
         (rest (state-turtle-stack a-state))
         (cons (turtle-verts (state-turt a-state))
               (state-branch-verts a-state))
         (cons (turtle-color (state-turt a-state))
               (state-colors a-state))
         (state-d a-state)
         (state-beta a-state)))

(define global-interp
  (match-lambda
    ['F (lambda (state) (move-forward-and-draw state))]
    ['- (lambda (state) (rotate-right state))]
    ['+ (lambda (state) (rotate-left state))]
    ['\[ (lambda (state) (push-turtle state))]
    ['\] (lambda (state) (pop-turtle state))]
    [x (lambda (x) x)]))