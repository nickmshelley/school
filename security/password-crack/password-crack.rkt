#lang racket
(require ffi/unsafe)

(define crypt (get-ffi-obj "crypt" "libcrypt" (_fun _string _string -> _string)))

;(crypt "hellohi" "$6$F")

(define (write-user-line user)
  (define to-write
    (string-append user ":x:::::"))
  (write-string to-write passwd-out)
  (newline passwd-out))

(define (write-shadow-line user password)
  (define to-write
    (string-append user ":" (crypt password "$1$F") ":::::::"))
  (write-string to-write shadow-out)
  (newline shadow-out))

(define in (open-input-file "users.txt"))
(define passwd-out (open-output-file "passwd" #:exists 'replace))
(define shadow-out (open-output-file "shadow" #:exists 'replace))

(for ([line (in-port read-line in)])
  (define splitted (regexp-split " " line))
  (write-user-line (first splitted))
  (write-shadow-line (first splitted) (second splitted)))

(close-output-port passwd-out)
(close-output-port shadow-out)