#!r6rs

(import (rnrs)
        (scheme-tools)
        (sine utils)
        (rename (sine spn)
                (marginalize sine-marginalize))
        (rename (sine external cosh)
                (marginalize cosh-marginalize)))

(define query-expr
  '(begin
     (query
      (define x (flip))
      (define y (flip))
      x
      (or x y))))

(define (main)
  (let ([sine-marginal (sine-marginalize query-expr)]
        [cosh-marginal (cosh-marginalize query-expr)])
    (pe "sine:\n")
    (for-each pretty-print (log-marginal->marginal sine-marginal))
    (pe "\ncosh:\n")
    (for-each pretty-print (log-marginal->marginal cosh-marginal))))

(main)