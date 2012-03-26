#!r6rs

(import (rnrs)
        (scheme-tools)
        (sine interpreter))

(define test-prog
  '(begin
     (define x 3)
     (define f (lambda (x) (if (flip)
                          (* x x)
                          x)))
     (f x)))

(display (repeat 10 (lambda () (sicp-interpreter test-prog))))