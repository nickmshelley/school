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

(define ELEMS 10)
(define SPREAD 5)
(define matrix
  (for/list ([i (in-range ELEMS)])
    (for/list ([j (in-range ELEMS)])
      (* 2 (random)))))

(define (init-opengl)
  (gl-clear-color 1 1 1 1)
  
  (gl-light-v 'light0 'position (gl-float-vector 1 1 1 0))
  
  (gl-enable 'lighting)
  (gl-enable 'light0)
  (gl-depth-func 'lequal)
  (gl-enable 'depth-test))

(define (matrix-ref matrix i j)
  (list-ref (list-ref matrix i) j))

(define (draw-matrix)
  (define delta (/ 10 ELEMS))
  (gl-material-v 'front 'diffuse (gl-float-vector .1 .2 .8 1))
  (gl-begin 'triangle-strip)
  (gl-normal 0 1 0)
  (for ([i (in-range (sub1 ELEMS))]
        [z (in-range (- SPREAD) (- SPREAD delta) delta)])
    ;take care of first 4 poinst which consist of 2 triangles
    (define left (- SPREAD))
    ;lower-left
    (gl-vertex left (matrix-ref matrix i 0) z)
    ;upper-left
    (gl-vertex left (matrix-ref matrix (add1 i) 0) (+ delta z))
    ;lower-right
    (gl-vertex (+ delta left) (matrix-ref matrix (add1 i) 1) z)
    ;upper-right
    (gl-vertex (+ delta left) (matrix-ref matrix (add1 i) 1) (+ delta z))
    (for ([j (in-range 2 ELEMS)]
          [x (in-range (+ (* 2 delta) left) SPREAD delta)])
      ;just do this row's and the next row's vertex because it's triangle strips
      (gl-normal 0 (abs x) 0)
      (gl-vertex x (matrix-ref matrix i j) z)
      (gl-vertex x (matrix-ref matrix (add1 i) j) (+ z delta))))
  (gl-end))

(define (draw-opengl)
  (gl-clear 'color-buffer-bit 'depth-buffer-bit)
  (gl-push-matrix)
  (gluLookAt -7 5 16
             .5 0 0
             0 1 0)
  (draw-matrix)
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