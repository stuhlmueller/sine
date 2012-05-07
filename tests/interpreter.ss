#!r6rs

(import (rnrs)
        (scheme-tools)
        (sine interpreter))

(define test-prog
  '(begin
     (define x 3)
     (define y (list 'a (+ 5 5) true))
     (define (z a) (* a a))
     (define (fact x)
       (if (or (= x 1)
               (= x 0))
           x
           (* x (fact (- x 1)))))
     (begin
       (define f
         (lambda (x) (if (flip)
                    (* x x)
                    x)))
       (list (fact 4) (z (f x)) y))))

(display (repeat 10 (lambda () (sicp-interpreter test-prog))))