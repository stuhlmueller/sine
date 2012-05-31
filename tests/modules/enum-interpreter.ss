#!r6rs

(import (rnrs)
        (sine)
        (scheme-tools))

(define test-prog
  '(begin
     (define x 3)
     (define f (lambda (x)
                 (if (flip .2)
                     (* x x)
                     (if (flip .1)
                         17
                         x))))
     (f x)))

(define query-prog
  '(begin
     (query
      (define x (flip))
      true
      (flip .01))))

(for-each pretty-print
          (alist-map (lambda (v p) (cons v (exp p)))
                     (enum-interpreter query-prog)))