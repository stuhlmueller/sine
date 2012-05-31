#!r6rs

(import (rnrs)
        (scheme-tools)
        (sine utils)
        (sine spn))

(define trivial-expr
  '(repeat 5 flip))

(for-each pen (log-marginal->marginal (time (marginalize trivial-expr))))
