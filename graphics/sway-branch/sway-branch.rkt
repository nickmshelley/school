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

(define branch (list (vector -3 0 0)
                     (vector 3 0 0)))

(define (draw-line line)
  (gl-begin 'line-strip)
  (gl-color-v (gl-float-vector .3 .2 .4))
  (for ([vert line])
    (gl-vertex-v (vector->gl-float-vector vert)))
  (gl-end))

(define (draw-opengl)
  (gl-clear 'color-buffer-bit 'depth-buffer-bit)
  (gl-push-matrix)
  (gl-line-width 2)
  (gluLookAt 0 0 10
             0 0 0
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