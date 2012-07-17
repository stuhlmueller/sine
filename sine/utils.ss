#!r6rs

(library

 (sine utils)

 (export log-marginal->marginal
         sum-of-marginals
         vector-sum
         apply-subcall
         normalize-vector
         diff-dist-fdist
         sample-discrete/log
         safe-log1minus)

 (import (rnrs)
         (scheme-tools srfi-compat :1)
         (scheme-tools srfi-compat :43)
         (scheme-tools value-number)
         (scheme-tools math distributions)
         (scheme-tools math fragmented-distributions)
         (scheme-tools math)
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

 (define (diff-dist-fdist dist sub-fdist)
   (let-values ([(vals ps) (dist-vals&ps dist)])
     (make-dist vals
                (vector-map (lambda (v p) (log (- (exp p) (exp (get-fdist-prob sub-fdist v)))))
                            vals
                            ps))))

 (define (sample-discrete/log list-of-pairs)
   (let ([list-of-lists/exp (map (lambda (x) (list (car x) (exp (cdr x)))) list-of-pairs)])
     (apply multinomial (apply zip list-of-lists/exp))))

 (define (safe-log1minus v)
   (if (and (> v 0.0)
            (< v 0.000000000000001))
       (begin
         (pen "safe-log1minus: rounded " v " to 0.0")
         (safe-log1minus 0.0))
       (log (- 1.0 (exp v)))))

 )

