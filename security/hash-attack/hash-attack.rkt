#lang racket
(require openssl/sha1)

(define alphabet "abcdefghijklmnopqrstuvwxyz")

(sha1 (open-input-string "hi"))