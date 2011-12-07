#lang racket
(require ffi/unsafe)

(define crypt (get-ffi-obj "crypt" "libcrypt" (_fun _string _string -> _string)))

;(crypt "hellohi" "$6$F")

(define in (open-input-file "users.txt"))
(for ([line (in-port read-line in)])
  (printf "~v~n" line))