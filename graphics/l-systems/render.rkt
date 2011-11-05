#lang racket

(require "semantics.rkt"
         racket/gui
         racket/draw
         sgl
         sgl/gl
         sgl/gl-vectors)

(provide render)

;table lookup from char to string
(define koch-snowflake
  (match-lambda
    ['F '((F +  F - - F + F))]
    [x (list (list 1 (list x)))]))

(define plant
  (match-lambda
    ['F '(1 (F F - \[ - F + F + F \] + \[ + F - F - F \]))]
    [x (list (list 1 (list x)))]))

(define another-plant
  (match-lambda
    ['X '((1 ( F \[ + X \] \[ - X \] F X)))]
    ['F '((1 (F F)))]
    [x (list (list 1 (list x)))]))

(define random-plant
  (match-lambda
    ['F '((.33 (F \[ + F \] F \[ − F \] F))
          (.33 (F \[ + F \] F))
          (.34 (F \[ - F \] F)))]
    [x (list (list 1 (list x)))]))

#;(define vecs (reverse 
                (turtle-verts 
                 (state-turt 
                  (turtle-eval koch-snowflake-interp 
                               (state (turtle 0 0 (* 60 0.0174532925) (list (vector 0 0 0)))
                                      empty
                                      empty
                                      .05
                                      60)
                               (eval-lsys koch-snowflake 5 '(F - - F - - F)))))))

(define final-state (turtle-eval global-interp
                                 (state (turtle 0 0 (* 90 0.0174532925) (list (vector 0 0 0)) (vector .3 .1 .3))
                                        empty
                                        empty
                                        empty
                                        .1
                                        20)
                                 (eval-lsys random-plant 5 '(F))))

(define (init-opengl)
  (gl-clear-color 1 1 1 1)
  (gl-depth-func 'lequal)
  (gl-enable 'depth-test))

(define (vertex-from-vector vec)
  (gl-vertex-v (vector->gl-float-vector vec)))

(define (resize w h)
  (gl-matrix-mode 'projection)
  (gl-load-identity)
  (gluPerspective 45 1 1 100)
  (gl-matrix-mode 'modelview)
  (gl-viewport 0 0 w h)
  #t)

(define (draw-line line color)
  (gl-begin 'line-strip)
  (gl-color-v (vector->gl-float-vector color))
  (for ([vert line])
    (gl-vertex-v (vector->gl-float-vector vert)))
  (gl-end))


(define (draw-opengl lines colors)
  (gl-clear 'color-buffer-bit 'depth-buffer-bit)
  (gl-push-matrix)
  (gl-line-width 2)
  (gluLookAt 0 3 10
             0 3 0
             0 1 0)
  (map draw-line lines colors)
  (gl-pop-matrix))

(define my-canvas%
  (class* canvas% ()
    (inherit refresh with-gl-context swap-gl-buffers)
    
    (init-field lines)
    (init-field colors)
    
    (define/public (gl-init)
      (with-gl-context
       (lambda ()
         (init-opengl))))
    
    (define/override (on-paint)
      (with-gl-context
       (lambda ()
         (draw-opengl lines colors)
         (swap-gl-buffers)
         (queue-callback (lambda x (send this refresh)) #f))))
    
    (define/override (on-size width height)
      (with-gl-context
       (lambda ()
         (resize width height))))
    
    (super-instantiate () (style '(gl)))))

(define (render final-state)
  (define the-lines (cons (turtle-verts (state-turt final-state))
                          (state-branch-verts final-state)))
  (define the-colors (cons (turtle-color (state-turt final-state))
                           (state-colors final-state)))

  (define win (new frame% (label "OpenGl Test")))
  (define gl (new my-canvas% (parent win) (min-width 800) (min-height 800) (lines the-lines) (colors the-colors)))
  (send gl gl-init)
  (send win show #t))
