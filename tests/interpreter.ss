#!r6rs

(import (rnrs)
        (scheme-tools)
        (sine desugar-Y)
        (sine interpreter))

(define test-prog-norecursion
  '(begin
     (define x 3)
     (define y (list 'a (+ 5 5) true))
     (define (z a) (* a a))
     (define (blubb x)
       (if (or (= x 1)
               (= x 0))
           x
           (* x (- x 1))))
     (begin
       (define f
         (lambda (x) (if (flip)
                    (* x x)
                    x)))
       (list (blubb 4) (z (f x)) y))))

(define test-prog-recursion-Y
  '(begin
     (define fac
       (Y
        (lambda (f)
          (lambda (n)
            (if (= n 1)
                n
                (* n (f (- n 1))))))))
     (fac 30)))

(define test-prog-recursion
  '(begin
     (define fac
       (lambda (n)
         (if (= n 1)
             n
             (* n (fac (- n 1))))))
     (fac 30)))

(for-each pretty-print
          (repeat 10 (lambda () (sicp-interpreter test-prog-recursion))))