#lang racket

(require racket/gui
         racket/draw
         sgl
         sgl/gl
         sgl/gl-vectors)

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


(define c2 0.22187)
(define c4 0.754029)
(define (delta x)
  (+ (* (expt x 2) c2)
     (* (expt x 4) c4)))

(define (vertex-from-x x)
  (vector x (- (delta x)) 0))

(define branch (map vertex-from-x
                    (sequence->list (in-range 0 1 .001))))

(define divisor 80)
(define op -)
(define (draw-line line)
  (gl-begin 'line-strip)
  (gl-color-v (gl-float-vector .3 .2 .4))
  (when (> divisor 100)
    (set! op -))
  (when (<= divisor 8)
    (set! op +))
  (set! divisor (op divisor 1))
  (for ([vert line])
    (gl-vertex-v (vector->gl-float-vector 
                  (vector (vector-ref vert 0)
                          (/ (vector-ref vert 1) divisor)
                          (vector-ref vert 2)))))
  (gl-end))

(define (draw-opengl)
  (gl-clear 'color-buffer-bit 'depth-buffer-bit)
  (gl-push-matrix)
  (gl-line-width 2)
  (gluLookAt 0 0 2
             .5 0 0
             0 1 0)
  (draw-line branch)
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
(define gl (new my-canvas% (parent win) (min-width 800) (min-height 800)))
(send gl gl-init)
(send win show #t)