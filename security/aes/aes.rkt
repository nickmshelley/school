#lang racket
(require rackunit)

(define sbox (bytes
              #x63 #x7C #x77 #x7B #xF2 #x6B #x6F #xC5 #x30 #x01 #x67 #x2B #xFE #xD7 #xAB #x76
              #xCA #x82 #xC9 #x7D #xFA #x59 #x47 #xF0 #xAD #xD4 #xA2 #xAF #x9C #xA4 #x72 #xC0
              #xB7 #xFD #x93 #x26 #x36 #x3F #xF7 #xCC #x34 #xA5 #xE5 #xF1 #x71 #xD8 #x31 #x15
              #x04 #xC7 #x23 #xC3 #x18 #x96 #x05 #x9A #x07 #x12 #x80 #xE2 #xEB #x27 #xB2 #x75
              #x09 #x83 #x2C #x1A #x1B #x6E #x5A #xA0 #x52 #x3B #xD6 #xB3 #x29 #xE3 #x2F #x84
              #x53 #xD1 #x00 #xED #x20 #xFC #xB1 #x5B #x6A #xCB #xBE #x39 #x4A #x4C #x58 #xCF
              #xD0 #xEF #xAA #xFB #x43 #x4D #x33 #x85 #x45 #xF9 #x02 #x7F #x50 #x3C #x9F #xA8
              #x51 #xA3 #x40 #x8F #x92 #x9D #x38 #xF5 #xBC #xB6 #xDA #x21 #x10 #xFF #xF3 #xD2
              #xCD #x0C #x13 #xEC #x5F #x97 #x44 #x17 #xC4 #xA7 #x7E #x3D #x64 #x5D #x19 #x73
              #x60 #x81 #x4F #xDC #x22 #x2A #x90 #x88 #x46 #xEE #xB8 #x14 #xDE #x5E #x0B #xDB
              #xE0 #x32 #x3A #x0A #x49 #x06 #x24 #x5C #xC2 #xD3 #xAC #x62 #x91 #x95 #xE4 #x79
              #xE7 #xC8 #x37 #x6D #x8D #xD5 #x4E #xA9 #x6C #x56 #xF4 #xEA #x65 #x7A #xAE #x08
              #xBA #x78 #x25 #x2E #x1C #xA6 #xB4 #xC6 #xE8 #xDD #x74 #x1F #x4B #xBD #x8B #x8A
              #x70 #x3E #xB5 #x66 #x48 #x03 #xF6 #x0E #x61 #x35 #x57 #xB9 #x86 #xC1 #x1D #x9E
              #xE1 #xF8 #x98 #x11 #x69 #xD9 #x8E #x94 #x9B #x1E #x87 #xE9 #xCE #x55 #x28 #xDF
              #x8C #xA1 #x89 #x0D #xBF #xE6 #x42 #x68 #x41 #x99 #x2D #x0F #xB0 #x54 #xBB #x16))

(define (state-ref state r c)
  (bytes-ref (vector-ref state c) r))

(define (print-state state)
  (for ([i (in-range 4)])
    (for ([j (in-range 4)])
      (printf "~x " (state-ref state i j)))
    (printf "~n")))

(define (printb ...)
  (printf "~b" ...))

(define (printx ...)
  (printf "~x~n" ...))

; xtime : num -> num
; argument n is assumed to be < 16 (ie a byte)
; performs a left shift followed by a conditional xor with #x11b
(define (xtime n)
  (define intermediate (arithmetic-shift n 1))
  (if (> n 128)
      (bitwise-xor #x11b intermediate)
      intermediate))

(define (mult num prod)
  (if (zero? prod) 0
      (if (even? prod)
          (mult (xtime num) (arithmetic-shift prod -1))
          (bitwise-xor num (mult (xtime num) (arithmetic-shift prod -1))))))
(check-equal? (mult #x57 0) 0)
(check-equal? (mult #x57 1) #x57)
(check-equal? (mult #x57 2) #xae)
(check-equal? (mult #x57 3) (bitwise-xor #x57 #xae))
(check-equal? (mult #x57 #x13) #xfe)

(define (substitute val)
  (bytes-ref sbox (+ (* (arithmetic-shift val -4) 16) (modulo val 16))))
(check-equal? (substitute 0) #x63)
(check-equal? (substitute #x3a) #x80)

(define (sub-bytes state)
  (for/vector ([c (in-vector state)])
    (list->bytes 
     (for/list ([v (in-bytes c)])
       (substitute v)))))
(check-equal? (sub-bytes (vector (bytes #x19 #x3d #xe3 #xbe)
                                 (bytes #xa0 #xf4 #xe2 #x2b)
                                 (bytes #x9a #xc6 #x8d #x2a)
                                 (bytes #xe9 #xf8 #x48 #x08)))
              (vector (bytes #xd4 #x27 #x11 #xae)
                      (bytes #xe0 #xbf #x98 #xf1)
                      (bytes #xb8 #xb4 #x5d #xe5)
                      (bytes #x1e #x41 #x52 #x30)))

(define (shift-rows state)
  (for/vector ([i (in-range (vector-length state))])
    (bytes
     (state-ref state 0 i)
     (state-ref state 1 (modulo (+ i 1) 4))
     (state-ref state 2 (modulo (+ i 2) 4))
     (state-ref state 3 (modulo (+ i 3) 4)))))
(check-equal? (shift-rows (vector (bytes #xd4 #x27 #x11 #xae)
                                  (bytes #xe0 #xbf #x98 #xf1)
                                  (bytes #xb8 #xb4 #x5d #xe5)
                                  (bytes #x1e #x41 #x52 #x30)))
              (vector (bytes #xd4 #xbf #x5d #x30)
                      (bytes #xe0 #xb4 #x52 #xae)
                      (bytes #xb8 #x41 #x11 #xf1)
                      (bytes #x1e #x27 #x98 #xe5)))

; mix-columns : state -> state
(define (mix-columns state)
  (for/vector #:length (vector-length state)
    ([c (in-vector state)])
    (bytes
     (bitwise-xor (mult (bytes-ref c 0) 2)
                  (mult (bytes-ref c 1) 3)
                  (bytes-ref c 2)
                  (bytes-ref c 3))
     
     (bitwise-xor (mult (bytes-ref c 1) 2)
                  (mult (bytes-ref c 2) 3)
                  (bytes-ref c 0)
                  (bytes-ref c 3))
     
     (bitwise-xor (mult (bytes-ref c 2) 2)
                  (mult (bytes-ref c 3) 3)
                  (bytes-ref c 0)
                  (bytes-ref c 1))
     
     (bitwise-xor (mult (bytes-ref c 3) 2)
                  (mult (bytes-ref c 0) 3)
                  (bytes-ref c 1)
                  (bytes-ref c 2)))))

(define (add-round-key state round-key)
  (for/vector ([state-col (in-vector state)]
               [key-col (in-vector round-key)])
    (list->bytes
     (for/list ([state-val (in-bytes state-col)]
                [key-val (in-bytes key-col)])
       (bitwise-xor key-val state-val)))))
(check-equal? (

;key-expand : vector -> vector
;takes a key (as a vector of bytes) and returns an expanded vector of bytes (w array in spec)
#;(define (key-expand key)
    (define num-rounds (+ (vector-length key) 6))
    (vector-append
     key
     (for/vector )))

(define (cipher input key)
  (define w (key-expand key))
  (sub-bytes input))

(cipher (vector (bytes #xd4 #xbf #x5d #x30)
                (bytes #xe0 #xb4 #x52 #xae)
                (bytes #xb8 #x41 #x11 #xf1)
                (bytes #x1e #x27 #x98 #xe5)))