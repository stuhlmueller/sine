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

(for-each pretty-print
          (enum-interpreter test-prog))