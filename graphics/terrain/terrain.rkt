#lang racket
(require racket/gui
         racket/draw
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

(define (argb->rgb l)
  (for/list ([i (in-range (length l))]
             [elem l]
             #:unless (= (modulo i 4) 0))
    elem))

;(define textures (glGenTextures 3))
;(define shrub-texture (gl-vector-ref textures 0))
;(define stone-texture (gl-vector-ref textures 1))
;(define mountain-texture (gl-vector-ref textures 2))
;
;(define (generate-textures)
;  (glBindTexture GL_TEXTURE_2D shrub-texture)
;  (define shrubbery (make-bitmap 512 512))
;  (send shrubbery load-file "shrubbery.jpg" 'jpeg)
;  (define shrub-bytes (make-bytes (* 512 512 4)))
;  (send shrubbery get-argb-pixels 0 0 512 512 shrub-bytes)
;  (define shrub-pixels (list->gl-byte-vector (reverse (bytes->list shrub-bytes))))
;  (glTexImage2D GL_TEXTURE_2D 0 3 512 512 0 GL_BGRA GL_UNSIGNED_BYTE shrub-pixels)
;  (gl-pixel-store 'unpack-alignment 1)
;  (glTexParameterf GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST)
;  (glTexParameterf GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST)
;  (gl-enable 'texture-2d)
;  
;  (glBindTexture GL_TEXTURE_2D stone-texture)
;  (define stone (make-bitmap 512 512))
;  (send stone load-file "stone.jpg" 'jpeg)
;  (define stone-bytes (make-bytes (* 512 512 4)))
;  (send stone get-argb-pixels 0 0 512 512 stone-bytes)
;  (define stone-pixels (list->gl-byte-vector (reverse (bytes->list stone-bytes))))
;  (glTexImage2D GL_TEXTURE_2D 0 3 512 512 0 GL_BGRA GL_UNSIGNED_BYTE stone-pixels)
;  (gl-pixel-store 'unpack-alignment 1)
;  (glTexParameterf GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST)
;  (glTexParameterf GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST)
;  (gl-enable 'texture-2d)
;  
;  (glBindTexture GL_TEXTURE_2D mountain-texture)
;  (define mountain (make-bitmap 1024 1024))
;  (send mountain load-file "mountain.png")
;  (define mountain-bytes (make-bytes (* 1024 1024 4)))
;  (send mountain get-argb-pixels 0 0 1024 1024 mountain-bytes)
;  (define mountain-pixels (list->gl-byte-vector (reverse (bytes->list mountain-bytes))))
;  (glTexImage2D GL_TEXTURE_2D 0 3 1024 1024 0 GL_BGRA GL_UNSIGNED_BYTE mountain-pixels)
;  (gl-pixel-store 'unpack-alignment 1)
;  (glTexParameterf GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST)
;  (glTexParameterf GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST)
;  (gl-enable 'texture-2d))

(define (init-opengl)
  (gl-clear-color 1 1 1 1)
  (gl-light-v 'light0 'position (gl-float-vector 0 0 1 0))
  (gl-shade-model 'smooth)
  (gl-enable 'lighting)
  (gl-enable 'light0)
  (gl-depth-func 'lequal)
  (gl-enable 'depth-test)
  ;(generate-textures)
  #;(glBindTexture GL_TEXTURE_2D stone-texture))

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

(define (get-color p n)
  ;default
  (gl-float-vector 1 1 1 1)
  ;elevation
  (if (< (point-y p) 0)
        (gl-float-vector .2 .7 .3 1)
        (gl-float-vector .6 .3 .2 1))
  ;slope
  #;(if (> (gl-vector-ref n 1) .7)
        (gl-float-vector .2 .7 .3 1)
        (gl-float-vector .7 .8 .8 1))
  ;aspect
  #;(if (> (gl-vector-ref n 0) .6)
        (gl-float-vector .2 .7 .3 1)
        (gl-float-vector .7 .8 .8 1)))

;(define current-texture -1)
#;(define (get-texture p n)
  ;default
  shrub-texture
  ;elevation
  (if (< (point-y p) 0)
        shrub-texture
        mountain-texture)
  ;slope
  #;(if (> (gl-vector-ref n 1) .7)
        shrub-texture
        mountain-texture)
  ;aspect
  #;(if (> (gl-vector-ref n 0) .6)
        shrub-texture
        mountain-texture))

(define (draw-points center-point top-point)
  (define center-normal (point-normal center-point))
  (gl-normal-v center-normal)
  (gl-material-v 'front 'ambient-and-diffuse 
                 (get-color center-point center-normal))
  (gl-tex-coord (point-x center-point) (point-z center-point))
  (gl-vertex-v (vector-from-point center-point))
  
  (define top-normal (point-normal top-point))
  (gl-normal-v top-normal)
  (gl-material-v 'front 'ambient-and-diffuse 
                 (get-color top-point top-normal))
  (gl-tex-coord (point-x top-point) (point-z top-point))
  (gl-vertex-v (vector-from-point top-point)))

#;(define (draw-points-with-texture left-point top-left-point center-point top-point)
  (define center-normal (point-normal center-point))
  (define new-texture (get-texture center-point center-normal))
  (unless (= current-texture new-texture)
    (set! current-texture new-texture)
    (gl-end)
    (glBindTexture GL_TEXTURE_2D current-texture)
    (gl-begin 'triangle-strip)
    (draw-points left-point top-left-point))
  (draw-points center-point top-point))

(define (draw-matrix)
  (reset-normals matrix)
  (for ([j (in-range (sub1 ELEMS))])
    ;(set! current-texture -1)
    (gl-begin 'triangle-strip)
    (for ([i (in-range 0 ELEMS)])
      (define zero-vector (gl-float-vector 0 0 0))
      (define center-point (matrix-ref matrix i j))
      (define top-point (matrix-ref matrix i (add1 j)))
      (define center (vector-from-point center-point))
      (define top (vector-from-point top-point))
      (define top-top (if (< j (- ELEMS 2))
                          (vector-from-point (matrix-ref matrix i (+ 2 j)))
                          #f))
      (define-values (left-point left)
        (if (> i 0)
            (values (matrix-ref matrix (sub1 i) j)
                    (vector-from-point (matrix-ref matrix (sub1 i) j)))
            (values #f #f)))
      (define top-top-left (if (and (< j (- ELEMS 2)) (> i 1))
                               (vector-from-point (matrix-ref matrix (sub1 i) (+ 2 j)))
                               #f))
      (define right (if (< i (sub1 ELEMS))
                        (vector-from-point (matrix-ref matrix (add1 i)  j))
                        #f))
      (define top-right (if (< i (sub1 ELEMS))
                            (vector-from-point (matrix-ref matrix (add1 i) (add1 j)))
                            #f))
      (define bottom (if (> j 0)
                         (vector-from-point (matrix-ref matrix i (sub1 j)))
                         #f))
      (define-values (top-left-point top-left)
        (if (> i 0)
            (values (matrix-ref matrix (sub1 i) (add1 j))
                    (vector-from-point (matrix-ref matrix (sub1 i) (add1 j))))
            (values #f #f)))
      (define bottom-right (if (and (> j 1) (< i (sub1 ELEMS)))
                               (vector-from-point (matrix-ref matrix (add1 i) (sub1 j)))
                               #f))
      
      (unless (point-normal center-point)
        (set-point-normal! center-point (point-up (normalize (add-vectors (list (normal-from-vectors center left top-left)
                                                                                (normal-from-vectors center top-left top)
                                                                                (normal-from-vectors center top right)
                                                                                (normal-from-vectors center right bottom-right)
                                                                                (normal-from-vectors center bottom-right bottom)
                                                                                (normal-from-vectors center bottom left)))))))
      (unless (point-normal top-point)
        (set-point-normal! top-point (point-up (normalize (add-vectors (list (normal-from-vectors top top-left top-top-left)
                                                                             (normal-from-vectors top top-top-left top-top)
                                                                             (normal-from-vectors top top-top top-right)
                                                                             (normal-from-vectors top top-right right)
                                                                             (normal-from-vectors top right center)
                                                                             (normal-from-vectors top center top-left)))))))
      (draw-points center-point top-point)
      #;(when (> i 0)
        (if (< i (sub1 ELEMS))
            (draw-points-with-texture left-point top-left-point center-point top-point)
            (draw-points center-point top-point))))
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
(define smooth-scale .1)
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
(define upper-left-field (instantiate text-field% ("upper-left" update-panel) (init-value "2")))
(define upper-right-field (instantiate text-field% ("upper-right" update-panel) (init-value "2")))
(define lower-left-field (instantiate text-field% ("lower-left" update-panel) (init-value "0")))
(define lower-right-field (instantiate text-field% ("lower-right" update-panel) (init-value "0")))
(define height-delta-field (instantiate text-field% ("max height delta" update-panel) (init-value "3")))
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