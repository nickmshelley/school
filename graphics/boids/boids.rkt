#lang racket

(require racket/gui
         racket/draw
         sgl
         sgl/gl
         sgl/gl-vectors)

(struct boid (center velocity) #:transparent #:mutable)

(define (init-opengl)
  (gl-clear-color 1 1 1 1)
  (gl-depth-func 'lequal)
  (gl-enable 'depth-test))

(define (resize w h)
  (gl-matrix-mode 'projection)
  (gl-load-identity)
  (gluPerspective 45 1 .1 200)
  (gl-matrix-mode 'modelview)
  (gl-viewport 0 0 w h)
  #t)

(define WORLD-SIZE 40)
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

(define NUM-BOIDS 100)
(define OFFSET .2)
(define (make-boids)
  (for/list ([i (in-range NUM-BOIDS)])
    (boid (vector (* OFFSET i) 0 0)
          (vector .1 .2 0))))
(define boids (make-boids))

(define NUM-OBSTACLES 50)
(define (make-obstacles)
  (for/list ([i (in-range NUM-OBSTACLES)])
    (boid (vector (- (* (random) 2 WORLD-SIZE) WORLD-SIZE)
                  (- (* (random) 2 WORLD-SIZE) WORLD-SIZE)
                  0)
          (vector 0 0 0))))
(define obstacles (make-obstacles))

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

(define (square num)
  (expt num 2))

(define (distance-between v1 v2)
  (sqrt (+ (square (- (vector-ref v1 0)
                      (vector-ref v2 0)))
           (square (- (vector-ref v1 1)
                      (vector-ref v2 1)))
           (square (- (vector-ref v1 2)
                      (vector-ref v2 2))))))

(define BUBBLE 5)
(define (visible? b other)
  (define distance (distance-between (boid-center b) 
                                     (boid-center other)))
  (and (> distance 0.001) (< distance BUBBLE)))

(define (negate-vector v)
  (vector (- (vector-ref v 0))
          (- (vector-ref v 1))
          (- (vector-ref v 2))))

(define (scalar-mult v s)
  (vector (* s (vector-ref v 0))
          (* s (vector-ref v 1))
          (* s (vector-ref v 2))))

(define (scalar-div v s)
  (scalar-mult v (/ 1 s)))

(define (add-vectors v1 v2)
  (vector (+ (vector-ref v1 0)
             (vector-ref v2 0))
          (+ (vector-ref v1 1)
             (vector-ref v2 1))
          (+ (vector-ref v1 2)
             (vector-ref v2 2))))

(define (sub-vectors v1 v2)
  (vector (- (vector-ref v1 0)
             (vector-ref v2 0))
          (- (vector-ref v1 1)
             (vector-ref v2 1))
          (- (vector-ref v1 2)
             (vector-ref v2 2))))

(define (inverse-sub-vectors v1 v2)
  (scalar-mult
   (sub-vectors v1 v2)
   (/ 1 (distance-between v1 v2))))

(define (vector-magnitude v)
  (sqrt (+ (square (vector-ref v 0))
           (square (vector-ref v 1))
           (square (vector-ref v 2)))))

(define MAX-VELOCITY .5)
(define (dampen v)
  (define magnitude (vector-magnitude v))
  (if (> magnitude MAX-VELOCITY)
      (scalar-mult v (/ MAX-VELOCITY magnitude))
      v))

(define S .3)
(define K .4)
(define M .2)
(define R .1)
(define (update-boid-velocity b)
  (define visible-birds (filter (lambda (other) 
                                  (visible? b other))
                                boids))
  (define visible-obstacles (filter (lambda (other)
                                      (visible? b other))
                                    obstacles))
  (define visibles (append visible-birds visible-obstacles))
  (define avoid-centers (map boid-center visibles))
  (define attract-centers (map boid-center visible-birds))
  (define velocities (map boid-velocity visible-birds))
  (define separation 
    (foldl add-vectors
           (vector 0 0 0)
           (map (lambda (other)
                  (inverse-sub-vectors (boid-center b) other))
                avoid-centers)))
  (define cohesion
    (if (> (length attract-centers) 0)
        (sub-vectors 
         (scalar-div (foldl add-vectors (vector 0 0 0) attract-centers)
                     (length attract-centers))
         (boid-center b))
        (vector 0 0 0)))
  (define alignment 
    (if (> (length velocities) 0)
        (scalar-div (foldl add-vectors (vector 0 0 0) velocities)
                    (length velocities))
        (vector 0 0 0)))
  (define changes (list (scalar-mult separation S)
                        (scalar-mult cohesion K)
                        (scalar-mult alignment M)))
  (define new-velocity 
    (foldl add-vectors (boid-velocity b) changes))
  (set-boid-velocity! b (dampen new-velocity))
  (define rand (vector (- .5 (random)) (- .5 (random)) 0))
  (set-boid-velocity! b (dampen 
                         (add-vectors (boid-velocity b) 
                                      (scalar-mult rand R)))))


(define (update-boid-pos b)
  (set-boid-center! b 
                    (keep-inside
                     (add-vectors (boid-center b) (boid-velocity b)))))

(define (update-boids)
  (map update-boid-velocity boids)
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

(define OBST-SIZE 1)
(define (draw-obstacle o)
  (define center (boid-center o))
  (define x (vector-ref center 0))
  (define y (vector-ref center 1))
  (gl-polygon-mode 'front-and-back 'fill)
  (gl-begin 'quads)
  (gl-color .8 .2 .2)
  (gl-vertex (- x OBST-SIZE) (- y OBST-SIZE) 0)
  (gl-vertex (+ x OBST-SIZE) (- y OBST-SIZE) 0)
  (gl-vertex (+ x OBST-SIZE) (+ y OBST-SIZE) 0)
  (gl-vertex (- x OBST-SIZE) (+ y OBST-SIZE) 0)
  (gl-end))

(define (draw-opengl)
  (gl-clear 'color-buffer-bit 'depth-buffer-bit)
  (gl-push-matrix)
  (gluLookAt 0 0 100
             0 0 0
             0 1 0)
  (draw-border)
  (map draw-boid boids)
  (map draw-obstacle obstacles)
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
(define gl (new my-canvas% (parent win) (min-width 1300) (min-height 1300)))
(send gl gl-init)
(send win show #t)