#lang racket/gui
(require sgl
         sgl/gl-vectors)

(define (resize w h)
  (gl-viewport 0 0 w h)
  #t)

(define (init-opengl)
  (gl-clear-color 0 0 0 1)
  (gl-color 1 1 1)
  (gl-ortho -50 50 -50 50 -1 1)
  (gl-shade-model 'flat))

(define spin 0.0)

(define (draw-opengl)
  (define new-spin (+ spin 2.0))
  (set! spin (if (> new-spin 360) (- new-spin 360) new-spin))
  
  (gl-clear 'color-buffer-bit 'depth-buffer-bit)
  (gl-push-matrix)
  (gl-rotate spin 0 0 -1)
  (gl-rect -25 -25 25 25)
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