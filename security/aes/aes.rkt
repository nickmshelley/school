#lang racket

(define state (vector #xd4 #xbf #x5d #x30 
                      #xe0 #xb4 #x52 #xae
                      #xb8 #x41 #x11 #xf1
                      #x1e #x27 #x98 #xe5))

(define (print-state s)
  (printf "~x ~x ~x ~x~n~x ~x ~x ~x~n~x ~x ~x ~x~n~x ~x ~x ~x~n"
          (vector-ref s 0) (vector-ref s 4) (vector-ref s 8) (vector-ref s 12)
          (vector-ref s 1) (vector-ref s 5) (vector-ref s 9) (vector-ref s 13)
          (vector-ref s 2) (vector-ref s 6) (vector-ref s 10) (vector-ref s 14)
          (vector-ref s 3) (vector-ref s 7) (vector-ref s 11) (vector-ref s 15)))

(define mx #x1b)
(define num #x57)
(define num2 #x13)
(define (printb ...)
  (printf "~b" ...))
(define (printx ...)
  (printf "~x~n" ...))

(define (state-ref s r c)
  (vector-ref s (+ (* c 4) r)))

(define (state-set! s r c v)
  (vector-set! s (+ (* c 4) r) v))

; xtime : num -> num
; argument n is assumed to be < 16
; performs a left shift followed by a conditional xor with #x1b
(define (xtime n)
  (define intermediate (arithmetic-shift n 1))
  (if (> n 128)
      (bitwise-xor #x11b intermediate)
      intermediate))

(define (apply-xtime num times)
  (if (= times 1)
      (xtime num)
      (apply-xtime (xtime num) (sub1 times))))

;prod is assumed to be either 2 or 3
(define (weird-mult num prod)
  (cond
    [(= prod 2) (xtime num)]
    [(= prod 3) (bitwise-xor num (xtime num))]
    [else (print "unrecognized number!")]))

; mix-columns : state -> state
(define (mix-columns s)
  (define new-state (vector-copy s))
  (for ([c (in-range 4)])
    (state-set! new-state 0 c
                (bitwise-xor (weird-mult (state-ref s 0 c) 2)
                             (weird-mult (state-ref s 1 c) 3)
                             (state-ref s 2 c)
                             (state-ref s 3 c)))
    (state-set! new-state 1 c 
                (bitwise-xor (weird-mult (state-ref s 1 c) 2)
                             (weird-mult (state-ref s 2 c) 3)
                             (state-ref s 0 c)
                             (state-ref s 3 c)))
    (state-set! new-state 2 c
                (bitwise-xor (weird-mult (state-ref s 2 c) 2)
                             (weird-mult (state-ref s 3 c) 3)
                             (state-ref s 0 c)
                             (state-ref s 1 c)))
    (state-set! new-state 3 c
                (bitwise-xor (weird-mult (state-ref s 3 c) 2)
                             (weird-mult (state-ref s 0 c) 3)
                             (state-ref s 1 c)
                             (state-ref s 2 c))))
  new-state)
  