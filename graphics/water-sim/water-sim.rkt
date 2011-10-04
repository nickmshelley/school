#lang racket/gui
(require sgl
         sgl/gl
         sgl/gl-vectors)

(define (resize w h)
  (gl-matrix-mode 'projection)
  (gl-load-identity)
  (gluPerspective 45 1 .1 100)
  (gl-matrix-mode 'modelview)
  (gl-viewport 0 0 w h)
  #t)

(define (init-opengl)
  (gl-clear-color 0 0 0 1)
  (gl-color 1 1 1)
  (gl-shade-model 'flat))

(define spin 0.0)

(define (draw-opengl)
  (gl-clear 'color-buffer-bit 'depth-buffer-bit)
  (gl-push-matrix)
  (gluLookAt -7 5 16
             0 0 0
             0 1 0)
  (set! spin (+ spin .01))
  (printf "spin: ~a~n" spin)
  (gl-begin 'quads)
  (gl-vertex 5 0 5)
  (gl-vertex 5 0 -5)
  (gl-vertex -5 0 -5)
  (gl-vertex -5 0 5)
  (gl-end)
  (gl-pop-matrix)
  (gl-end))

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

(define win (new frame% (label "OpenGl Test") (min-width 400) (min-height
                                                               400)))
(define gl  (new my-canvas% (parent win)))

(send gl gl-init)
(send win show #t)