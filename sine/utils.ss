#!r6rs

(library

 (sine utils)

 (export log-marginal->marginal
         sum-of-marginals
         vector-sum
         apply-recur
         normalize-vector)

 (import (rnrs)
         (scheme-tools srfi-compat :43)
         (scheme-tools value-number)
         (scheme-tools)
         (sine coroutine-interpreter))

 (define (log-marginal->marginal marginal)
   (alist-map (lambda (v p) (cons v (exp p)))
              marginal))

 (define (sum-of-marginals marginals)
   (sum (map cdr marginals)))

 (define (vector-sum vec)
   (let ([total 0])
     (vector-for-each (lambda (n) (set! total (+ total n))) vec)
     total))

 (define (normalize-vector vec)
   (let ([s (vector-sum vec)])
     (vector-map (lambda (x) (/ x s))
                 vec)))

 )

