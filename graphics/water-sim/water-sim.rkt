#lang racket/gui

(require (lib "gl.ss" "sgl")
         (lib "gl-vectors.ss" "sgl"))


(define (resize w h)
  (glViewport 0 0 w h)
  #t)

(define x 0.0)
(define fun +)

(define (draw-opengl)
  (glClearColor 0.0 0.0 0.0 0.0)
  (glClear GL_COLOR_BUFFER_BIT)
  (glColor3d 1.0 1.0 1.0)

  (glMatrixMode GL_PROJECTION)
  (glLoadIdentity)
  (glOrtho 0.0 1.0 0.0 1.0 -1.0 1.0)
  (glMatrixMode GL_MODELVIEW)
  (glLoadIdentity)

  (glBegin GL_QUADS)
  (glVertex3d x 0.25 0.0)
  (glVertex3d 0.75 0.25 0.0)
  (glVertex3d 0.75 0.75 0.0)
  (glVertex3d 0.25 0.75 0.0)
  (glEnd)
  (when (> x .5) (set! fun -))
  (when (< x .1) (set! fun +))
  (set! x (fun x 0.005)))


(define my-canvas%
  (class* canvas% ()
    (inherit refresh with-gl-context swap-gl-buffers)

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

(define win (new frame% (label "OpenGl Test") (min-width 200) (min-height
200)))
(define gl  (new my-canvas% (parent win)))

(send win show #t)