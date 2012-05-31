#!r6rs

(import (rnrs)
        (scheme-tools)
        (sine utils)
        (rename (sine spn)
                (marginalize sine-marginalize))
        (rename (sine external cosh)
                (marginalize cosh-marginalize)))

(define test-expr
  '(begin
     (define foo
       (lambda ()
         (let ([x (flip)]
               [y (flip)])
           (if x true (foo)))))
     (foo)))

(define (main)
  (let ([sine-marginal (sine-marginalize test-expr)]
        [cosh-marginal (cosh-marginalize test-expr)])
    (pe "sine:\n")
    (for-each pretty-print (log-marginal->marginal sine-marginal))
    (pe "\ncosh:\n")
    (for-each pretty-print (log-marginal->marginal cosh-marginal))))

(main)