#lang racket

(require racket/gui
         racket/draw
         sgl
         sgl/gl
         sgl/gl-vectors)

(struct boid (center velocity) #:transparent #:mutable)

(define (vector-add v1 v2)
  (vector (+ (vector-ref v1 0)
             (vector-ref v2 0))
          (+ (vector-ref v1 1)
             (vector-ref v2 1))
          (+ (vector-ref v1 2)
             (vector-ref v2 2))))

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

(define WORLD-SIZE 20)
(define (draw-border)
  (gl-polygon-mode 'front-and-back 'line)
  (gl-line-width (max (/ WORLD-SIZE 10) 3))
  (gl-begin 'quads)
  (gl-color 0 0 0)
  (gl-vertex (- WORLD-SIZE) (- WORLD-SIZE))
  (gl-vertex (- WORLD-SIZE) WORLD-SIZE)
  (gl-vertex WORLD-SIZE WORLD-SIZE)
  (gl-vertex WORLD-SIZE (- WORLD-SIZE))
  (gl-end))

(define NUM-BOIDS 1)
(define OFFSET .2)
(define (make-boids)
  (for/list ([i (in-range NUM-BOIDS)])
    (boid (vector 0 0 0)
          (vector .1 .2 0))))
(define boids (make-boids))

(define (wrap num)
  (cond
    [(< num (- WORLD-SIZE))
     (+ num (* 2 WORLD-SIZE))]
    [(> num WORLD-SIZE)
     (- num (* 2 WORLD-SIZE))]
    [else num]))

(define (keep-inside v)
  (vector (wrap (vector-ref v 0))
          (wrap (vector-ref v 1))
          (wrap (vector-ref v 2))))

(define (update-boid-pos b)
  (set-boid-center! b 
                    (keep-inside
                     (vector-add (boid-center b) (boid-velocity b)))))

(define (update-boids)
  (map update-boid-pos boids))

(define (draw-boid b)
  (define center (boid-center b))
  (define velocity (boid-velocity b))
  (define degrees (* 57.2957795 
                     (atan (vector-ref velocity 1) 
                           (vector-ref velocity 0))))
  (gl-polygon-mode 'front-and-back 'fill)
  (gl-push-matrix)
  (gl-translate (vector-ref center 0)
                (vector-ref center 1)
                (vector-ref center 2))
  (gl-rotate degrees 0 0 1)
  (gl-begin 'triangles)
  (gl-color .4 .4 1)
  (gl-vertex .5 0 0)
  (gl-vertex -.5 -.3 0)
  (gl-vertex -.5 .3 0)
  (gl-end)
  (gl-pop-matrix))

(define (draw-opengl)
  (gl-clear 'color-buffer-bit 'depth-buffer-bit)
  (gl-push-matrix)
  (gluLookAt 0 0 50
             0 0 0
             0 1 0)
  (draw-border)
  (map draw-boid boids)
  (gl-pop-matrix))

(define RATE 1/60)
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
         (define next-time 
           (+ (current-inexact-milliseconds) (* RATE 1000)))
         (update-boids)
         (draw-opengl)
         (swap-gl-buffers)
         (sync (alarm-evt next-time))
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