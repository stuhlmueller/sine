#!r6rs

(import (rnrs)
        (scheme-tools)
        (sine utils)
        (sine spn))

(define uniform-draw-expr
  '(uniform-draw (list 1 2 3 4 5 'foo 'bar 'baz)))

(for-each pen (log-marginal->marginal (time (marginalize uniform-draw-expr))))
