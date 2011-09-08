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

(define inverse-sbox (bytes
                      #x52 #x09 #x6A #xD5 #x30 #x36 #xA5 #x38 #xBF #x40 #xA3 #x9E #x81 #xF3 #xD7 #xFB
                      #x7C #xE3 #x39 #x82 #x9B #x2F #xFF #x87 #x34 #x8E #x43 #x44 #xC4 #xDE #xE9 #xCB
                      #x54 #x7B #x94 #x32 #xA6 #xC2 #x23 #x3D #xEE #x4C #x95 #x0B #x42 #xFA #xC3 #x4E
                      #x08 #x2E #xA1 #x66 #x28 #xD9 #x24 #xB2 #x76 #x5B #xA2 #x49 #x6D #x8B #xD1 #x25
                      #x72 #xF8 #xF6 #x64 #x86 #x68 #x98 #x16 #xD4 #xA4 #x5C #xCC #x5D #x65 #xB6 #x92
                      #x6C #x70 #x48 #x50 #xFD #xED #xB9 #xDA #x5E #x15 #x46 #x57 #xA7 #x8D #x9D #x84
                      #x90 #xD8 #xAB #x00 #x8C #xBC #xD3 #x0A #xF7 #xE4 #x58 #x05 #xB8 #xB3 #x45 #x06
                      #xD0 #x2C #x1E #x8F #xCA #x3F #x0F #x02 #xC1 #xAF #xBD #x03 #x01 #x13 #x8A #x6B
                      #x3A #x91 #x11 #x41 #x4F #x67 #xDC #xEA #x97 #xF2 #xCF #xCE #xF0 #xB4 #xE6 #x73
                      #x96 #xAC #x74 #x22 #xE7 #xAD #x35 #x85 #xE2 #xF9 #x37 #xE8 #x1C #x75 #xDF #x6E
                      #x47 #xF1 #x1A #x71 #x1D #x29 #xC5 #x89 #x6F #xB7 #x62 #x0E #xAA #x18 #xBE #x1B
                      #xFC #x56 #x3E #x4B #xC6 #xD2 #x79 #x20 #x9A #xDB #xC0 #xFE #x78 #xCD #x5A #xF4
                      #x1F #xDD #xA8 #x33 #x88 #x07 #xC7 #x31 #xB1 #x12 #x10 #x59 #x27 #x80 #xEC #x5F
                      #x60 #x51 #x7F #xA9 #x19 #xB5 #x4A #x0D #x2D #xE5 #x7A #x9F #x93 #xC9 #x9C #xEF
                      #xA0 #xE0 #x3B #x4D #xAE #x2A #xF5 #xB0 #xC8 #xEB #xBB #x3C #x83 #x53 #x99 #x61
                      #x17 #x2B #x04 #x7E #xBA #x77 #xD6 #x26 #xE1 #x69 #x14 #x63 #x55 #x21 #x0C #x7D))

(define rcon (bytes
              #x01 #x02 #x04 #x08 
              #x10 #x20 #x40 #x80 
              #x1B #x36 #x6C #xD8 
              #xAB #x4D #x9A #x2F 
              #x5E #xBC #x63 #xC6 
              #x97 #x35 #x6A #xD4 
              #xB3 #x7D #xFA #xEF 
              #xC5 #x91 #x39 #x72 
              #xE4 #xD3 #xBD #x61 
              #xC2 #x9F #x25 #x4A 
              #x94 #x33 #x66 #xCC 
              #x83 #x1D #x3A #x74 
              #xE8 #xCB #x8D))

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
  (if (>= n 128)
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

(define (substitute val sub-array)
  (bytes-ref sub-array (+ (* (arithmetic-shift val -4) 16) (modulo val 16))))
(check-equal? (substitute 0 sbox) #x63)
(check-equal? (substitute #x3a sbox) #x80)

(define (sub-word word sub-array)
  (list->bytes 
   (for/list ([v (in-bytes word)])
     (substitute v sub-array))))

(define (sub-bytes state sub-array)
  (for/vector ([c (in-vector state)])
    (sub-word c sub-array)))
(check-equal? (sub-bytes (vector (bytes #x19 #x3d #xe3 #xbe)
                                 (bytes #xa0 #xf4 #xe2 #x2b)
                                 (bytes #x9a #xc6 #x8d #x2a)
                                 (bytes #xe9 #xf8 #x48 #x08)) sbox)
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
(check-equal? (mix-columns (vector (bytes #xfe #x7c #x7e #x71)
                                   (bytes #xfe #x7f #x80 #x70)
                                   (bytes #x47 #xb9 #x51 #x93)
                                   (bytes #xf6 #x7b #x8e #x4b)))
              (vector (bytes #x6c #xf5 #xed #xf9)
                      (bytes #x96 #xeb #x0a #x06)
                      (bytes #x9c #x4e #xf2 #x1c)
                      (bytes #xbf #xc2 #x57 #x62)))


(define (add-round-key state round-key)
  (for/vector ([state-col (in-vector state)]
               [key-col (in-vector round-key)])
    (list->bytes
     (for/list ([state-val (in-bytes state-col)]
                [key-val (in-bytes key-col)])
       (bitwise-xor key-val state-val)))))
(check-equal? (add-round-key 
               (vector (bytes #x32 #x43 #xf6 #xa8) ;input
                       (bytes #x88 #x5a #x30 #x8d)
                       (bytes #x31 #x31 #x98 #xa2)
                       (bytes #xe0 #x37 #x07 #x34))
               (vector (bytes #x2b #x7e #x15 #x16) ;key
                       (bytes #x28 #xae #xd2 #xa6)
                       (bytes #xab #xf7 #x15 #x88)
                       (bytes #x09 #xcf #x4f #x3c)))
              (vector (bytes #x19 #x3d #xe3 #xbe)
                      (bytes #xa0 #xf4 #xe2 #x2b)
                      (bytes #x9a #xc6 #x8d #x2a)
                      (bytes #xe9 #xf8 #x48 #x08)))

(define (rotate-word word)
  (list->bytes
   (for/list ([i (in-range (bytes-length word))])
     (bytes-ref word (modulo (add1 i) (bytes-length word))))))
(check-equal? (rotate-word (bytes 1 2 3 4)) (bytes 2 3 4 1))

(define (xor-word word1 word2)
  (list->bytes
   (for/list ([i (in-range (bytes-length word1))])
     (bitwise-xor (bytes-ref word1 i) (bytes-ref word2 i)))))
(check-equal? (xor-word (bytes #xa0 #xfa #xfe #x17) (bytes #x28 #xae #xd2 #xa6))
              (bytes #x88 #x54 #x2c #xb1))

;key-expand : vector -> vector
;takes a key (as a vector of bytes) and returns an expanded vector of bytes (w array in spec)
(define (key-expand key)
  (define key-length (vector-length key))
  (define num-rounds (+ key-length 6))
  (for/fold ([vec key])
    ([i (in-range (- (* 4 (add1 num-rounds)) key-length))])
    (define temp (vector-ref vec (sub1 (vector-length vec))))
    (vector-append 
     vec
     (vector 
      (xor-word 
       (vector-ref vec i)
       (cond
         [(= (modulo i key-length) 0) 
          (xor-word (sub-word (rotate-word temp) sbox) (bytes (bytes-ref rcon (/ i key-length)) 0 0 0))]
         [(and (> key-length 6) (= (modulo i key-length) 4))
          (sub-word temp sbox)]
         [else temp]))))))

(define (cipher input key)
  (define num-rounds (+ (vector-length key) 6))
  (define w-init (key-expand key))
  ;    (printf "~nround key:~n") 
  ;    (print-state (vector-take-right w-init 4))
  ;  (printf "~nstate:~n")
  ;  (print-state (add-round-key input (vector-take w-init 4)))
  (define-values
    (w-out state-out)
    (for/fold ([w (vector-take-right w-init (- (vector-length w-init) 4))]
               [state (add-round-key input (vector-take w-init 4))])
      ([i (in-range (sub1 num-rounds))])
      ;      (printf "~nsub bytes:~n") 
      ;      (print-state (sub-bytes state))
      ;      (printf "~nshift rows:~n")
      ;      (print-state (shift-rows (sub-bytes state)))
      ;      (printf "~nmix columns:~n")
      ;      (print-state (mix-columns (shift-rows (sub-bytes state))))
      ;      (printf "~nround key:~n")
      ;      (print-state (vector-take w 4))
      ;      (printf "~nstate:~n")
      ;      (print-state (add-round-key (mix-columns (shift-rows (sub-bytes state)))
      ;                                  (vector-take w 4)))
      (values 
       (vector-take-right w (- (vector-length w) 4))
       (add-round-key (mix-columns (shift-rows (sub-bytes state sbox)))
                      (vector-take w 4)))))
  (add-round-key (shift-rows (sub-bytes state-out sbox))
                 w-out))

(check-equal? (cipher (vector (bytes #x32 #x43 #xf6 #xa8) ;input
                              (bytes #x88 #x5a #x30 #x8d)
                              (bytes #x31 #x31 #x98 #xa2)
                              (bytes #xe0 #x37 #x07 #x34))
                      (vector (bytes #x2b #x7e #x15 #x16) ;key
                              (bytes #x28 #xae #xd2 #xa6)
                              (bytes #xab #xf7 #x15 #x88)
                              (bytes #x09 #xcf #x4f #x3c)))
              (vector (bytes #x39 #x25 #x84 #x1d)
                      (bytes #x02 #xdc #x09 #xfb)
                      (bytes #xdc #x11 #x85 #x97)
                      (bytes #x19 #x6a #x0b #x32)))

(check-equal? (cipher (vector (bytes #x00 #x11 #x22 #x33)
                              (bytes #x44 #x55 #x66 #x77)
                              (bytes #x88 #x99 #xaa #xbb)
                              (bytes #xcc #xdd #xee #xff))
                      (vector (bytes #x00 #x01 #x02 #x03)
                              (bytes #x04 #x05 #x06 #x07)
                              (bytes #x08 #x09 #x0a #x0b)
                              (bytes #x0c #x0d #x0e #x0f)))
              (vector (bytes #x69 #xc4 #xe0 #xd8)
                      (bytes #x6a #x7b #x04 #x30)
                      (bytes #xd8 #xcd #xb7 #x80)
                      (bytes #x70 #xb4 #xc5 #x5a)))

(check-equal? (cipher (vector (bytes #x00 #x11 #x22 #x33)
                              (bytes #x44 #x55 #x66 #x77)
                              (bytes #x88 #x99 #xaa #xbb)
                              (bytes #xcc #xdd #xee #xff))
                      (vector (bytes #x00 #x01 #x02 #x03)
                              (bytes #x04 #x05 #x06 #x07)
                              (bytes #x08 #x09 #x0a #x0b)
                              (bytes #x0c #x0d #x0e #x0f)
                              (bytes #x10 #x11 #x12 #x13)
                              (bytes #x14 #x15 #x16 #x17)))
              (vector (bytes #xdd #xa9 #x7c #xa4)
                      (bytes #x86 #x4c #xdf #xe0)
                      (bytes #x6e #xaf #x70 #xa0)
                      (bytes #xec #x0d #x71 #x91)))

(check-equal? (cipher (vector (bytes #x00 #x11 #x22 #x33)
                              (bytes #x44 #x55 #x66 #x77)
                              (bytes #x88 #x99 #xaa #xbb)
                              (bytes #xcc #xdd #xee #xff))
                      (vector (bytes #x00 #x01 #x02 #x03)
                              (bytes #x04 #x05 #x06 #x07)
                              (bytes #x08 #x09 #x0a #x0b)
                              (bytes #x0c #x0d #x0e #x0f)
                              (bytes #x10 #x11 #x12 #x13)
                              (bytes #x14 #x15 #x16 #x17)
                              (bytes #x18 #x19 #x1a #x1b)
                              (bytes #x1c #x1d #x1e #x1f)))
              (vector (bytes #x8e #xa2 #xb7 #xca)
                      (bytes #x51 #x67 #x45 #xbf)
                      (bytes #xea #xfc #x49 #x90)
                      (bytes #x4b #x49 #x60 #x89)))

(define (inverse-shift-rows state)
  (for/vector ([i (in-range (vector-length state))])
    (bytes
     (state-ref state 0 i)
     (state-ref state 1 (modulo (- i 1) 4))
     (state-ref state 2 (modulo (- i 2) 4))
     (state-ref state 3 (modulo (- i 3) 4)))))

(define (inverse-mix-columns state)
  (for/vector #:length (vector-length state)
    ([c (in-vector state)])
    (bytes
     (bitwise-xor (mult (bytes-ref c 0) #xe)
                  (mult (bytes-ref c 1) #xb)
                  (mult (bytes-ref c 2) #xd)
                  (mult (bytes-ref c 3) #x9))
     (bitwise-xor (mult (bytes-ref c 0) #x9)
                  (mult (bytes-ref c 1) #xe)
                  (mult (bytes-ref c 2) #xb)
                  (mult (bytes-ref c 3) #xd))
     (bitwise-xor (mult (bytes-ref c 0) #xd)
                  (mult (bytes-ref c 1) #x9)
                  (mult (bytes-ref c 2) #xe)
                  (mult (bytes-ref c 3) #xb))
     (bitwise-xor (mult (bytes-ref c 0) #xb)
                  (mult (bytes-ref c 1) #xd)
                  (mult (bytes-ref c 2) #x9)
                  (mult (bytes-ref c 3) #xe)))))

(define (inverse-cipher input key)
  (define num-rounds (+ (vector-length key) 6))
  (define w-init (key-expand key))
  ;  (printf "vector length: ~a~n" (vector-length w-init))
  ;  (printf "~nround key:~n") 
  ;  (print-state (vector-take-right w-init 4))
  ;  (printf "~nstate:~n")
  ;  (print-state (add-round-key input (vector-take-right w-init 4)))
  (define-values
    (w-out state-out)
    (for/fold ([w (vector-take w-init (- (vector-length w-init) 4))]
               [state (add-round-key input (vector-take-right w-init 4))])
      ([i (in-range (sub1 num-rounds))])
      ;      (printf "~nshift rows:~n")
      ;      (print-state (inverse-shift-rows state))
      ;      (printf "~nsub bytes:~n")
      ;      (print-state (sub-bytes (inverse-shift-rows state) inverse-sbox))
      ;      (printf "~nround key:~n")
      ;      (print-state (vector-take-right w 4))
      ;      (printf "~nadd round key:~n")
      ;      (print-state (add-round-key (sub-bytes (inverse-shift-rows state) inverse-sbox)
      ;                                  (vector-take-right w 4)))
      ;      (printf "~nmix columns:~n")
      ;      (print-state (inverse-mix-columns (add-round-key (sub-bytes (inverse-shift-rows state) inverse-sbox)
      ;                                                       (vector-take-right w 4))))
      (values 
       (vector-take w (- (vector-length w) 4))
       (inverse-mix-columns (add-round-key (sub-bytes (inverse-shift-rows state) inverse-sbox)
                                           (vector-take-right w 4))))))
  (add-round-key (sub-bytes (inverse-shift-rows state-out) inverse-sbox)
                 w-out))

(check-equal? (inverse-cipher (vector (bytes #x69 #xc4 #xe0 #xd8)
                                      (bytes #x6a #x7b #x04 #x30)
                                      (bytes #xd8 #xcd #xb7 #x80)
                                      (bytes #x70 #xb4 #xc5 #x5a))
                              (vector (bytes #x00 #x01 #x02 #x03)
                                      (bytes #x04 #x05 #x06 #x07)
                                      (bytes #x08 #x09 #x0a #x0b)
                                      (bytes #x0c #x0d #x0e #x0f)))
              (vector (bytes #x00 #x11 #x22 #x33)
                      (bytes #x44 #x55 #x66 #x77)
                      (bytes #x88 #x99 #xaa #xbb)
                      (bytes #xcc #xdd #xee #xff)))

(check-equal? (inverse-cipher (vector (bytes #xdd #xa9 #x7c #xa4)
                                      (bytes #x86 #x4c #xdf #xe0)
                                      (bytes #x6e #xaf #x70 #xa0)
                                      (bytes #xec #x0d #x71 #x91))
                              (vector (bytes #x00 #x01 #x02 #x03)
                                      (bytes #x04 #x05 #x06 #x07)
                                      (bytes #x08 #x09 #x0a #x0b)
                                      (bytes #x0c #x0d #x0e #x0f)
                                      (bytes #x10 #x11 #x12 #x13)
                                      (bytes #x14 #x15 #x16 #x17)))
              (vector (bytes #x00 #x11 #x22 #x33)
                      (bytes #x44 #x55 #x66 #x77)
                      (bytes #x88 #x99 #xaa #xbb)
                      (bytes #xcc #xdd #xee #xff)))

(check-equal? (inverse-cipher (vector (bytes #x8e #xa2 #xb7 #xca)
                                      (bytes #x51 #x67 #x45 #xbf)
                                      (bytes #xea #xfc #x49 #x90)
                                      (bytes #x4b #x49 #x60 #x89))
                              (vector (bytes #x00 #x01 #x02 #x03)
                                      (bytes #x04 #x05 #x06 #x07)
                                      (bytes #x08 #x09 #x0a #x0b)
                                      (bytes #x0c #x0d #x0e #x0f)
                                      (bytes #x10 #x11 #x12 #x13)
                                      (bytes #x14 #x15 #x16 #x17)
                                      (bytes #x18 #x19 #x1a #x1b)
                                      (bytes #x1c #x1d #x1e #x1f)))
              (vector (bytes #x00 #x11 #x22 #x33)
                      (bytes #x44 #x55 #x66 #x77)
                      (bytes #x88 #x99 #xaa #xbb)
                      (bytes #xcc #xdd #xee #xff)))