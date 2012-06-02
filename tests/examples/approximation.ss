#!r6rs

(import (rnrs)
        (scheme-tools)
        (sine utils)
        (sine spn))

(define approx-expr
  '(flip))

(for-each (lambda (max-spn-size)
            (pen "\nmax-spn-size: " max-spn-size)
            (for-each pen (log-marginal->marginal (marginalize approx-expr 'max-spn-size max-spn-size))))
          (list 1 2 3 4 5))
