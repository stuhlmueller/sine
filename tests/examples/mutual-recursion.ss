#!r6rs

(import (rnrs)
        (scheme-tools)
        (sine utils)
        (sine spn))

(define mutual-recursion-expr
  '(begin

     (define even*
       (lambda (even odd)
         (lambda (n)
           (if (= n 0)
               true
               (odd (- n 1))
               ))))

     (define odd*
       (lambda (even odd)
         (lambda (n)
           (if (= n 0)
               false
               (even (- n 1))
               ))))

     (let* ([fs (Y* even* odd*)]
            [even (car fs)]
            [odd (cadr fs)])
       (even 10))

     ))

(for-each pen (log-marginal->marginal (time (marginalize mutual-recursion-expr))))
