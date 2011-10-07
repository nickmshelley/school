#lang racket
(require racket/gui
         sgl
         sgl/gl
         sgl/gl-vectors)

(struct point (x h z u v normal) #:mutable #:transparent)

(define ELEMS 25)
(define SPREAD 5)
(define START-HEIGHT 10)
(define delta (/ (* 2 SPREAD) ELEMS))

(define (vector-from-point v)
  (gl-float-vector (point-x v)
                   (+ START-HEIGHT (point-h v))
                   (point-z v)))

(define (reset-normal p)
  (set-point-normal! p #f))

(define matrix
  (for/list ([i (in-range ELEMS)]
             [x (in-range (- SPREAD) SPREAD delta)])
    (for/list ([j (in-range ELEMS)]
               [z (in-range (- SPREAD) SPREAD delta)])
      (point x (random) z 0 0 #f))))

(define (resize w h)
  (gl-matrix-mode 'projection)
  (gl-load-identity)
  (gluPerspective 45 1 .1 100)
  (gl-matrix-mode 'modelview)
  (gl-viewport 0 0 w h)
  #t)

(define (init-opengl)
  (gl-clear-color 1 1 1 1)
  
  (gl-light-v 'light0 'position (gl-float-vector 1 1 1 0))
  
  (gl-shade-model 'smooth)
  (gl-enable 'lighting)
  (gl-enable 'light0)
  (gl-depth-func 'lequal)
  (gl-enable 'depth-test))

(define (matrix-ref matrix i j)
  (list-ref (list-ref matrix i) j))

(define (reset-normals mat)
  (for* ([i (in-range ELEMS)]
         [j (in-range ELEMS)])
    (reset-normal (matrix-ref mat i j))))

(define (get-vector-elements vec)
  (values (gl-vector-ref vec 0)
          (gl-vector-ref vec 1)
          (gl-vector-ref vec 2)))

(define (normalize vec)
  (define-values (x y z) (get-vector-elements vec))
  (define sum-length (sqrt (+ (expt x 2) (expt y 2) (expt z 2))))
  (gl-float-vector (/ x sum-length)
                   (/ y sum-length)
                   (/ z sum-length)))

(define (sub-vectors v1 v2)
  (define-values (ax ay az) (get-vector-elements v1))
  (define-values (bx by bz) (get-vector-elements v2))
  (gl-float-vector (- ax bx)
                   (- ay by)
                   (- az bz)))

;cross-prod : gl-vector gl-vector -> gl-float-vector
(define (cross-prod vec1 vec2)
  (define-values (x1 y1 z1) (get-vector-elements vec1))
  (define-values (x2 y2 z2) (get-vector-elements vec2))
  (gl-float-vector (- (* y1 z2) (* z1 y2))
                   (- (* z1 x2) (* x1 z2))
                   (- (* x1 y2) (* y1 x2))))

;add-vectors : listof gl-vector -> gl-vector
(define (add-vectors vec-list)
  (list->gl-float-vector
   (apply map + (map gl-vector->list vec-list))))

(define (point-up vec)
  (define-values (x y z) (get-vector-elements vec))
  (if (< y 0)
      (gl-float-vector (- x) (- y) (- z))
      vec))

(define (normal-from-vectors v1 v2 v3)
  (if (and v1 v2 v3)
      (cross-prod (sub-vectors v1 v2) 
                  (sub-vectors v2 v3))
      (gl-float-vector 0 0 0)))

(define (draw-matrix)
  (reset-normals matrix)
  (gl-material-v 'front-and-back 'ambient-and-diffuse (gl-float-vector .3 .4 .9 1))
  (gl-begin 'triangle-strip)
  (for* ([i (in-range (sub1 ELEMS))] 
         [j (in-range ELEMS)])
    (define zero-vector (gl-float-vector 0 0 0))
    (define center-point (matrix-ref matrix i j))
    (define top-point (matrix-ref matrix (add1 i) j))
    (define center (vector-from-point center-point))
    (define top (vector-from-point top-point))
    (define top-top (if (< i (- ELEMS 2))
                        (vector-from-point (matrix-ref matrix (+ 2 i) j))
                        #f))
    (define left (if (> j 0)
                     (vector-from-point (matrix-ref matrix i (sub1 j)))
                     #f))
    (define top-top-left (if (and (< i (- ELEMS 2)) (> 0 j))
                             (vector-from-point (matrix-ref matrix (+ 2 i) (sub1 j)))
                             #f))
    (define right (if (< j (sub1 ELEMS))
                      (vector-from-point (matrix-ref matrix i (add1 j)))
                      #f))
    (define top-right (if (< j (sub1 ELEMS))
                          (vector-from-point (matrix-ref matrix (add1 i) (add1 j)))
                          #f))
    (define bottom (if (> i 0)
                       (vector-from-point (matrix-ref matrix (sub1 i) j))
                       #f))
    (define top-left (if (> j 0)
                         (vector-from-point (matrix-ref matrix (add1 i) (sub1 j)))
                         #f))
    (define bottom-right (if (and (> i 0) (< j (sub1 ELEMS)))
                             (vector-from-point (matrix-ref matrix (sub1 i) (add1 j)))
                             #f))
    (define center-normal (if (point-normal center-point)
                              (point-normal center-point)
                              (normalize (add-vectors (list (normal-from-vectors center left top-left)
                                                            (normal-from-vectors center top-left top)
                                                            (normal-from-vectors center top right)
                                                            (normal-from-vectors center right bottom-right)
                                                            (normal-from-vectors center bottom-right bottom)
                                                            (normal-from-vectors center bottom left))))))
    (gl-normal-v (point-up center-normal))
    (define-values (a b c) (get-vector-elements center-normal))
    ;(printf "center normal: (~a ~a ~a)~n" a b c) 
    (gl-vertex-v center)
    (define top-normal (if (point-normal top-point)
                             (point-normal top-point)
                             (normalize (add-vectors (list (normal-from-vectors top top-left top-top-left)
                                                           (normal-from-vectors top top-top-left top-top)
                                                           (normal-from-vectors top top-top top-right)
                                                           (normal-from-vectors top top-right right)
                                                           (normal-from-vectors top right center)
                                                           (normal-from-vectors top center top-left))))))
    (gl-normal-v (point-up top-normal))
    (gl-vertex-v top))
  (gl-end))

(define (draw-opengl)
  (gl-clear 'color-buffer-bit 'depth-buffer-bit)
  (gl-push-matrix)
  (gluLookAt -7 (+ 5 START-HEIGHT) 16
             0 START-HEIGHT 0
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