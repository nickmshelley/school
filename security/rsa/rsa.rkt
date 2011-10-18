#lang racket
(require rackunit)

(define p 11082409523320287007916082974445113721119077054851707548874892754297681927938515345038340655142459351011837455389031882121411610712958995861026309126596083)
(define q 12970783625666488612822090886747644362936933969305251068789928213154748073086040884011900150522223042526134797297225237366907981879194050033638359176805903)
(define n (* p q))
(define t (* (sub1 p) (sub1 q)))
(define e 65537)

(define (modular-expt base exponent mod)
  (let loop ([result 1]
             [b base]
             [e exponent])
    (if (<= e 0)
        result
        (loop (if (odd? e) (modulo (* result b) mod) result)
              (modulo (* b b) mod)
              (arithmetic-shift e -1)))))
(check-equal? (modular-expt 3 2 5) 4)
(check-equal? (modular-expt 2 8 255) 1)
(check-equal? (modular-expt 5 
                            3599131035634557106248430806148785487095757694641533306480604458089470064537190296255232548883112685719936728506816716098566612844395439751206812144692131084107776
                            3660074356288457536446266188454355660772969140105659199256265853827624419172622053002165243290437080198739732869838099522030942643173797810463969023951907291654997179)
              1522762728733093584246082731814213802527903499438711000272371780935430413616451728877444975627039674525704095180070460137758887414132987926843395392785313225395231515)

(define (extended-gcd a b)
  (if (= b 0)
      (values 1 0)
      (let*-values ([(q r) (quotient/remainder a b)]
                   [(s t) (extended-gcd b r)])
        (values t (- s (* q t))))))
(define-values (t1 t2) (extended-gcd 120 23))
(check-equal? t1 -9)
(check-equal? t2 47)

(define d (let-values ([(_ d) (extended-gcd t e)])
            (if (negative? d)
                (+ d t)
                d)))

(define m 12345)
(check-equal? (modular-expt (modular-expt m e n) d n)
              m)
