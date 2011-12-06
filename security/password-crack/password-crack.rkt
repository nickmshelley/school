#lang racket
(require ffi/unsafe)

(define crypt (get-ffi-obj "crypt" "libcrypt" (_fun _string _string -> _string)))

(crypt "hellohi" "$6$F")