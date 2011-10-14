#lang racket
(require racket/gui
         data/queue
         sgl
         sgl/gl
         sgl/gl-vectors
         "sgl-vectors-utils.rkt")

(struct point (x y z normal) #:mutable #:transparent)

(define ELEMS 65)
(define SPREAD 5)
(define delta (/ (* 2 SPREAD) ELEMS))

(define (vector-from-point p)
  (gl-float-vector (point-x p)
                   (point-y p)
                   (point-z p)))

(define (reset-normal p)
  (set-point-normal! p #f))

(define (empty-matrix)
  (for/list ([i (in-range ELEMS)]
             [x (in-range (- SPREAD) SPREAD delta)])
    (for/list ([j (in-range ELEMS)]
               [z (in-range (- SPREAD) SPREAD delta)])
      (point x 0 z #f))))

(define matrix (empty-matrix))

(define (reset)
  (set! matrix (empty-matrix)))

(define (resize w h)
  (gl-matrix-mode 'projection)
  (gl-load-identity)
  (gluPerspective 45 1 .1 100)
  (gl-matrix-mode 'modelview)
  (gl-viewport 0 0 w h)
  #t)

(define (init-opengl)
  (gl-clear-color 1 1 1 1)
  
  (gl-light-v 'light0 'position (gl-float-vector 0 0 1 0))
  
  (gl-shade-model 'smooth)
  (gl-enable 'lighting)
  (gl-enable 'light0)
  (gl-depth-func 'lequal)
  (gl-enable 'depth-test))

; i and j are x and y
(define (matrix-ref mat i j)
  (list-ref (list-ref mat j) i))

(define (in-range? num)
  (and (>= num 0) (< num ELEMS)))

(define (safe-matrix-ref mat i j)
  (if (and (in-range? i) (in-range? j))
      (matrix-ref mat i j)
      #f))

(define (reset-normals mat)
  (for* ([i (in-range ELEMS)]
         [j (in-range ELEMS)])
    (reset-normal (matrix-ref mat i j))))

(define (draw-matrix)
  (reset-normals matrix)
  (gl-material-v 'front 'ambient-and-diffuse (gl-float-vector .3 .7 .2 1))
  (for ([j (in-range 1 (sub1 ELEMS))])
    (gl-begin 'triangle-strip)
    (for ([i (in-range 1 ELEMS)])
      (define zero-vector (gl-float-vector 0 0 0))
      (define center-point (matrix-ref matrix i j))
      (define top-point (matrix-ref matrix i (add1 j)))
      (define center (vector-from-point center-point))
      (define top (vector-from-point top-point))
      (define top-top (if (< j (- ELEMS 2))
                          (vector-from-point (matrix-ref matrix i (+ 2 j)))
                          #f))
      (define left (if (> i 1)
                       (vector-from-point (matrix-ref matrix (sub1 i) j))
                       #f))
      (define top-top-left (if (and (< j (- ELEMS 2)) (> i 1))
                               (vector-from-point (matrix-ref matrix (sub1 i) (+ 2 j)))
                               #f))
      (define right (if (< i (sub1 ELEMS))
                        (vector-from-point (matrix-ref matrix (add1 i)  j))
                        #f))
      (define top-right (if (< i (sub1 ELEMS))
                            (vector-from-point (matrix-ref matrix (add1 i) (add1 j)))
                            #f))
      (define bottom (if (> j 1)
                         (vector-from-point (matrix-ref matrix i (sub1 j)))
                         #f))
      (define top-left (if (> i 1)
                           (vector-from-point (matrix-ref matrix (sub1 i) (add1 j)))
                           #f))
      (define bottom-right (if (and (> j 1) (< i (sub1 ELEMS)))
                               (vector-from-point (matrix-ref matrix (add1 i) (sub1 j)))
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
    (gl-end)))

(define (draw-opengl)
  (gl-clear 'color-buffer-bit 'depth-buffer-bit)
  (gl-push-matrix)
  (gluLookAt 0 20 7
             0 0 0
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

(define (average point-list)
  (define height-list (map point-y (filter point? point-list)))
  (/ (apply + height-list)
     (length height-list)))

(define (rand-calc rand-amount)
  (- (* (random) rand-amount 2) rand-amount))

(define scale .5)
(define q (make-queue))

(define (run-stuff qu)
  (when (not (queue-empty? qu))
    ((dequeue! qu))
    (run-stuff qu)))

(define rough-scale 2)
(define smooth-scale .3)
(define (set-height! p ave rand-amount)
  (define to-add
    (rand-calc (if (< ave 0)
                   (* rand-amount smooth-scale)
                   (* rand-amount rough-scale))))
  (set-point-y! p (+ ave to-add)))

(define (midpoint-displace mat i j delta rand-amount)
  (define middle (matrix-ref mat i j))
  ;square step
  (define top-right (matrix-ref mat (+ i delta) (+ j delta)))
  (define top-left (matrix-ref mat (- i delta) (+ j delta)))
  (define bottom-right (matrix-ref mat (+ i delta) (- j delta)))
  (define bottom-left (matrix-ref mat (- i delta) (- j delta)))
  (set-height! middle 
               (average (list top-right top-left bottom-right bottom-left))
               rand-amount)
  ;diamond step
  (define right (matrix-ref mat (+ i delta) j))
  (define left (matrix-ref mat (- i delta) j))
  (define top (matrix-ref mat i (+ j delta)))
  (define bottom (matrix-ref mat i (- j delta)))
  (define right-right (safe-matrix-ref mat (+ i delta delta) j))
  (define left-left (safe-matrix-ref mat (- i delta delta) j))
  (define top-top (safe-matrix-ref mat i (+ j delta delta)))
  (define bottom-bottom (safe-matrix-ref mat i (- j delta delta)))
  (set-height! top
               (average (list middle top-left top-right top-top))
               rand-amount)
  (set-height! right
               (average (list middle top-right bottom-right right-right))
               rand-amount)
  (set-height! bottom
               (average (list middle bottom-left bottom-right bottom-bottom))
               rand-amount)
  (set-height! left
               (average (list middle top-left bottom-left left-left))
               rand-amount)
  ;recurse
  (define new-delta (/ delta 2))
  (define new-rand (* rand-amount scale))
  (when (> delta 1)
    (enqueue! q (lambda () (midpoint-displace mat (+ i new-delta) (+ j new-delta) new-delta new-rand)))
    (enqueue! q (lambda () (midpoint-displace mat (+ i new-delta) (- j new-delta) new-delta new-rand)))
    (enqueue! q (lambda () (midpoint-displace mat (- i new-delta) (+ j new-delta) new-delta new-rand)))
    (enqueue! q (lambda () (midpoint-displace mat (- i new-delta) (- j new-delta) new-delta new-rand)))))


(define (update-values upper-left upper-right lower-left lower-right rand-amount random-scale)
  (reset)
  (set-point-y! (matrix-ref matrix 0 0) upper-left)
  (set-point-y! (matrix-ref matrix 0 (sub1 ELEMS)) upper-right)
  (set-point-y! (matrix-ref matrix (sub1 ELEMS) 0) lower-left)
  (set-point-y! (matrix-ref matrix (sub1 ELEMS) (sub1 ELEMS)) lower-right)
  (set! scale random-scale)
  (define mid-index (/ (sub1 ELEMS) 2))
  (enqueue! q (lambda () (midpoint-displace matrix mid-index mid-index mid-index rand-amount)))
  (run-stuff q))

(define win (new frame% (label "OpenGl Test")))
(define gl  (new my-canvas% (parent win) (min-width 800) (min-height 800)))
(define main-panel (new horizontal-panel% (parent win)
                        (alignment '(center center)) (stretchable-height #f)))
(define update-panel (new vertical-panel% (parent main-panel)
                          (alignment '(center center))))
(define upper-left-field (instantiate text-field% ("upper-left" update-panel) (init-value "0")))
(define upper-right-field (instantiate text-field% ("upper-right" update-panel) (init-value "0")))
(define lower-left-field (instantiate text-field% ("lower-left" update-panel) (init-value "0")))
(define lower-right-field (instantiate text-field% ("lower-right" update-panel) (init-value "0")))
(define height-delta-field (instantiate text-field% ("max height delta" update-panel) (init-value "5")))
(define random-scale-field (instantiate text-field% ("random scale" update-panel) (init-value ".5")))
(instantiate button% ("Rebuild" main-panel (lambda (b e) (update-values (string->number (send upper-left-field get-value))
                                                                        (string->number (send upper-right-field get-value))
                                                                        (string->number (send lower-left-field get-value))
                                                                        (string->number (send lower-right-field get-value))
                                                                        (string->number (send height-delta-field get-value))
                                                                        (string->number (send random-scale-field get-value)))))
  (stretchable-width #f) (stretchable-height #t))

(send gl gl-init)
(send win show #t)