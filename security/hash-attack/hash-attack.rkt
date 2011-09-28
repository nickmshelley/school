#lang racket
(require openssl/sha1
         racket/generator
         test-engine/racket-tests)

(define (next-word current-word)
  (if (string=? current-word (make-string (string-length current-word) #\z))
      #f
      (list->string
       (let loop ([old-word (reverse (string->list current-word))]
                  [new-word empty]
                  [changed-non-z #f])
         (if (empty? old-word)
             new-word
             (if changed-non-z
                 (loop (rest old-word)
                       (cons (first old-word) new-word)
                       changed-non-z)
                 (if (char=? (first old-word) #\z)
                     (loop (rest old-word)
                           (cons #\a new-word)
                           changed-non-z)
                     (loop (rest old-word)
                           (cons (integer->char (add1 (char->integer (first old-word)))) new-word)
                           #t))))))))
(check-expect (next-word "a") "b")
(check-expect (next-word "at") "au")
(check-expect (next-word "cz") "da")
(check-expect (next-word "afzzz") "agaaa")
(check-expect (next-word "zzz") #f)

(define (make-generator size) 
  (generator ()
    (let loop ([word (make-string size #\a)])
      (yield word)
      (loop (next-word word)))))

(define (collision-attack digits)
  (define results (make-hash))
  (for* ([size (in-naturals 1)]
         [str (in-producer (make-generator size) #f)])
    (define result (hash-ref! results (substring (sha1 (open-input-string str)) 0 digits) str))
    (when (not (string=? result str)) (error 'collision-found "~a and ~a collided; hashes were:~n~a~n~a~nAttack took ~a steps"
                                             result str (sha1 (open-input-string result)) (sha1 (open-input-string str)) (length (hash-keys results))))))

(define (pre-image-attack character digits)
  (define image (make-string digits character))
  (define steps 0)
  (for* ([size (in-naturals 1)]
         [str (in-producer (make-generator size) #f)])
    (define result (substring (sha1 (open-input-string str)) 0 digits))
    (set! steps (add1 steps))
    (when (string=? result image) (raise steps))))

(define (pre-image-loop digits)
  (define steps-sum 0)
  (define seq "0123456789abcdef")
  (for ([c seq])
    (with-handlers 
        ([number? 
          (lambda (v) 
            (set! steps-sum (+ v steps-sum))
            (printf "steps: ~a~n" v))])
      (pre-image-attack c digits)))
  (printf "total: ~a~naverage: ~a~n" steps-sum (/ steps-sum 16.)))

;(collision-attack 5)

(pre-image-loop 4)

;(test)

