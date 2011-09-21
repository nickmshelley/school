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

(for* ([size (in-naturals 1)]
       [str (in-producer (make-generator size) #f)])
  (printf "~a~n" str #;(sha1 (open-input-string str))))

(test)


