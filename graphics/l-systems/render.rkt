#lang racket

(require "semantics.rkt"
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

(define (draw-line line color)
  (gl-begin 'line-strip)
  (gl-color-v (vector->gl-float-vector color))
  (for ([vert line])
    (gl-vertex-v (vector->gl-float-vector vert)))
  (gl-end))

(define (analyze lines)
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
  (values x-ave y-ave z-ave (max (* z-pos 1.3) 1)))

(define (draw-opengl lines colors x-ave y-ave z-ave z-pos)
  (gl-clear 'color-buffer-bit 'depth-buffer-bit)
  (gl-push-matrix)
  (gl-line-width 2)
  (gluLookAt x-ave y-ave z-pos
             x-ave y-ave z-ave
             0 1 0)
  (map draw-line lines colors)
  (gl-pop-matrix))

(define my-canvas%
  (class* canvas% ()
    (inherit refresh with-gl-context swap-gl-buffers)
    
    (init-field lines)
    (init-field colors)
    
    (define-values (x-ave y-ave z-ave z-pos)
      (analyze lines))
    
    (define/public (gl-init)
      (with-gl-context
       (lambda ()
         (init-opengl))))
    
    (define/override (on-paint)
      (with-gl-context
       (lambda ()
         (draw-opengl lines colors x-ave y-ave z-ave z-pos)
         (swap-gl-buffers)
         (queue-callback (lambda x (send this refresh)) #f))))
    
    (define/override (on-size width height)
      (with-gl-context
       (lambda ()
         (resize width height))))
    
    (super-instantiate () (style '(gl)))))

(define (render final-state)
  (define the-lines (cons (turtle-verts (state-turt final-state))
                          (state-branch-verts final-state)))
  (define the-colors (cons (turtle-color (state-turt final-state))
                           (state-colors final-state)))
  
  (define win (new frame% (label "L-Systems")))
  (define gl (new my-canvas% (parent win) (min-width 800) (min-height 800) (lines the-lines) (colors the-colors)))
  (send gl gl-init)
  (send win show #t))