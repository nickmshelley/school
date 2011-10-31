#lang s-exp "language.rkt"

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

(define (draw-opengl)
  (gl-clear 'color-buffer-bit 'depth-buffer-bit)
  (gl-push-matrix)
  (gl-begin line-strip)
  (interp (vertex-from-vector (F)) (plus) 
          (vertex-from-vector (F)) (plus) 
          (vertex-from-vector (F)) (plus)
          (vertex-from-vector (F)))
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