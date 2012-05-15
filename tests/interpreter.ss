#!r6rs

(import (rnrs)
        (scheme-tools)
        (scheme-tools srfi-compat :1)
        (sine interpreter)
        (scheme-tools profile))

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

(define (make-list-test n)
  `(begin
     (define make-list
       (lambda (n)
         (if (= n 0)
             '()
             (cons 5 (make-list (- n 1))))))
     (let ([L (make-list ,n)])
       'done)))

(define (profile-list-test)
  (pe "\"n\", \"time\"\n")
  (for-each (lambda (n)
              (pe n ", " (get-runtime (lambda () (sicp-interpreter (make-list-test n)))) "\n"))
            (map (lambda (x) (* x 1000)) '(1 2 3 4))))

(profile-list-test)