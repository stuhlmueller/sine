#!r6rs

(library

 (sine utils)

 (export log-marginal->marginal
         vector-sum
         apply-recur
         normalize-vector)

 (import (rnrs)
         (scheme-tools)
         (scheme-tools srfi-compat :43)
         (sine coroutine-interpreter)
         (sine value-number))

 (define (log-marginal->marginal marginal)
   (alist-map (lambda (v p) (cons v (exp p)))
              marginal))

 (define (vector-sum vec)
   (let ([total 0])
     (vector-for-each (lambda (n) (set! total (+ total n))) vec)
     total))

 (define (normalize-vector vec)
   (let ([s (vector-sum vec)])
     (vector-map (lambda (x) (/ x s))
                 vec)))

 (define (apply-recur recur)
   (let ([syntax+env (&expand-pair (recur-state recur))])
     (apply (recur-call recur)
            (list (car syntax+env)
                  (cdr syntax+env)))))

 )

