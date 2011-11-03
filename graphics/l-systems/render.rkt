#lang racket

(require rackunit
         racket/gui
         racket/draw
         sgl
         sgl/gl
         sgl/gl-vectors)

(struct turtle (x y angle))
(struct state (turt verts))

;table lookup from char to string
(define line
  (match-lambda
    ['F '(F +  F - - F + F)]
    [x (list x)]))

;eval-lsys : (symbol->listOfSymbol) nat listOfSymbol -> listOfSymbol
(define (eval-lsys produce generations axiom)
  (if (zero? generations)
      axiom
      (eval-lsys produce (sub1 generations) (append-map produce axiom))))

;turtle-eval : (symbol->function) state listOfSymbol -> state
(define (turtle-eval interps state string)
  (if (empty? string)
      state
      (turtle-eval interps
                   ((interps (first string)) state)
                   (rest string))))

(define (move-forward a-state)
  (define d .05)
  (define the-turtle (state-turt a-state))
  (state (turtle (+ (turtle-x the-turtle)
                    (* d (cos (turtle-angle the-turtle))))
                 (+ (turtle-y the-turtle)
                    (* d (sin (turtle-angle the-turtle))))
                 (turtle-angle the-turtle))
         (state-verts a-state)))

(define (rotate-left a-state)
  (define beta (* 60 0.0174532925))
  (define the-turtle (state-turt a-state))
  (state (turtle (turtle-x the-turtle)
                 (turtle-y the-turtle)
                 (+ (turtle-angle the-turtle) beta))
         (state-verts a-state)))

(define (rotate-right a-state)
  (define beta (* 60 0.0174532925))
  (define the-turtle (state-turt a-state))
  (state (turtle (turtle-x the-turtle)
                 (turtle-y the-turtle)
                 (- (turtle-angle the-turtle) beta))
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
    ['- (lambda (state) (rotate-right state))]
    ['+ (lambda (state) (rotate-left state))]
    [x (lambda (x) x)]))

(define vecs (reverse (state-verts (turtle-eval line-interp 
                                                (state (turtle 0 0 (* 60 0.0174532925)) (list (vector 0 0 0)))
                                                (eval-lsys line 5 '(F - - F - - F))))))

(define (init-opengl)
  (gl-clear-color 1 1 1 1)
  (gl-depth-func 'lequal)
  (gl-enable 'depth-test))

(define (vertex-from-vector vec)
  (gl-vertex-v (vector->gl-float-vector vec)))

(define (resize w h)
  (gl-matrix-mode 'projection)
  (gl-load-identity)
  (gluPerspective 45 1 .1 100)
  (gl-matrix-mode 'modelview)
  (gl-viewport 0 0 w h)
  #t)

(define (draw-opengl)
  (gl-clear 'color-buffer-bit 'depth-buffer-bit)
  (gl-push-matrix)
  (gluLookAt 0 0 20
             0 0 0
             0 1 0)
  (gl-begin 'line-strip)
  (gl-color 0 0 0)
  (for ([vec vecs])
    (gl-vertex-v (vector->gl-float-vector vec)))
  (gl-end)
  (gl-pop-matrix))

(define my-canvas%
  (class* canvas% ()
    (inherit refresh with-gl-context swap-gl-buffers)
    
    (define/public (gl-init)
      (with-gl-context
       (lambda ()
         (init-opengl))))
    
    (define/override (on-paint)
      (with-gl-context
       (lambda ()
         (draw-opengl)
         (swap-gl-buffers)
         (queue-callback (lambda x (send this refresh)) #f))))
    
    (define/override (on-size width height)
      (with-gl-context
       (lambda ()
         (resize width height))))
    
    (super-instantiate () (style '(gl)))))

(define win (new frame% (label "OpenGl Test")))
(define gl  (new my-canvas% (parent win) (min-width 800) (min-height 800)))

(send gl gl-init)
(send win show #t)