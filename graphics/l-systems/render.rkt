#lang racket

(require "semantics.rkt"
         "sgl-vectors-utils.rkt"
         racket/gui
         racket/draw
         sgl
         sgl/gl
         sgl/gl-vectors)

(provide render)

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

(define radius 0.01)
(define (cylinder-between-points a b)
  (define z (gl-float-vector 0 0 1))
  (define p (sub-vectors a b))
  (define p-len (vec-length p))
  (define t (cross-product z p))
  (define angle (* 57.29577951308232 
                   (acos (/ (dot-product z p) p-len))))
  (define-values (tx ty tz) (get-vector-elements b))
  (define-values (rx ry rz) (get-vector-elements t))
  (gl-push-matrix)
  (gl-translate tx ty tz)
  (gl-rotate angle rx ry rz)
  (gluSphere (gluNewQuadric) radius 40 40)
  (gluCylinder (gluNewQuadric) radius radius p-len 40 40)
  (gl-pop-matrix))

(define (draw-line line color)
  (gl-color-v (vector->gl-float-vector color))
  (define vec-line (map vector->gl-float-vector line))
  (for ([start vec-line]
        [end (rest vec-line)])
    (cylinder-between-points start end))
  (cylinder-between-points (last vec-line) (first vec-line)))

(define cam-x 0)
(define cam-y 0)
(define cam-z 2)
(define look-x 0)
(define look-y 0)
(define look-z -1)
(define rotate-angle 0)
(define delta-angle .01)
(define delta-pos .1)

(define (analyze! lines)
  (define xs 
    (append-map (lambda (v)
                  (map vector-ref v
                       (make-list (length v) 0))) 
                lines))
  (define ys
    (append-map (lambda (v)
                  (map vector-ref v
                       (make-list (length v) 1))) 
                lines))
  (define zs
    (append-map (lambda (v)
                  (map vector-ref v
                       (make-list (length v) 2))) 
                lines))
  (define x-ave (/ (+ (apply min xs)
                      (apply max xs))
                   2))
  (define y-ave (/ (+ (apply min ys)
                      (apply max ys))
                   2))
  (define z-ave (/ (+ (apply min zs)
                      (apply max zs))
                   2))
  (define z-pos (max (- (apply max xs) (apply min xs))
                     (- (apply max ys) (apply min ys))))
  (set! cam-x x-ave)
  (set! cam-y y-ave)
  (set! cam-z (max (* z-pos 1.3) 1))
  (set! delta-pos (* cam-z .05))
  (set! delta-angle (* delta-pos .1))
  (set! radius (* delta-pos .03)))

(define (draw-opengl lines colors)
  (gl-clear 'color-buffer-bit 'depth-buffer-bit)
  (gl-push-matrix)
  (gluLookAt cam-x cam-y cam-z
             (+ cam-x look-x) (+ cam-y look-y) (+ cam-z look-z)
             0 1 0)
  (map draw-line lines colors)
  (gl-pop-matrix))

(define (look-left)
  (set! rotate-angle (- rotate-angle delta-angle))
  (set! look-x (sin rotate-angle))
  (set! look-z (- (cos rotate-angle))))

(define (look-right)
  (set! rotate-angle (+ rotate-angle delta-angle))
  (set! look-x (sin rotate-angle))
  (set! look-z (- (cos rotate-angle))))

(define (go-forward)
  (set! cam-x (+ cam-x (* look-x delta-pos)))
  (set! cam-z (+ cam-z (* look-z delta-pos))))

(define (go-back)
  (set! cam-x (- cam-x (* look-x delta-pos)))
  (set! cam-z (- cam-z (* look-z delta-pos))))

(define (go-up)
  (set! cam-y (+ cam-y delta-pos)))

(define (go-down)
  (set! cam-y (- cam-y delta-pos)))

(define my-canvas%
  (class* canvas% ()
    (inherit refresh with-gl-context swap-gl-buffers)
    
    (init-field lines)
    (init-field colors)
    
    (define/public (gl-init)
      (with-gl-context
       (lambda ()
         (init-opengl))))
    
    (define/override (on-paint)
      (with-gl-context
       (lambda ()
         (draw-opengl lines colors)
         (swap-gl-buffers)
         (queue-callback (lambda x (send this refresh)) #f))))
    
    (define/override (on-size width height)
      (with-gl-context
       (lambda ()
         (resize width height))))
    
    (define/override (on-char event)
      (define ch (send event get-key-code))
      (match ch
        [#\a (look-left)]
        [#\d (look-right)]
        [#\s (go-back)]
        [#\w (go-forward)]
        [#\e (go-up)]
        [#\q (go-down)]
        [else (void)]))
    
    (super-instantiate () (style '(gl)))))

(define (render final-state)
  (define the-lines (cons (turtle-verts (state-turt final-state))
                          (state-branch-verts final-state)))
  (define the-colors (cons (turtle-color (state-turt final-state))
                           (state-colors final-state)))
  (analyze! the-lines)
  
  (define win (new frame% (label "L-Systems")))
  (define gl (new my-canvas% (parent win) (min-width 800) (min-height 800) (lines the-lines) (colors the-colors)))
  (send gl gl-init)
  (send win show #t))