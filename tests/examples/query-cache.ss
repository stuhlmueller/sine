#!r6rs

(import (rnrs)
        (scheme-tools)
        (sine utils)
        (sine spn))

(define query-cache-expr
  '(query
    (define x (flip))
    (define y (flip))
    x
    (or x y)))

(define query-nocache-expr
  '(query/nocache
    (define x (flip))
    (define y (flip))
    x
    (or x y)))

(for-each pen (log-marginal->marginal (time (marginalize query-cache-expr))))
(for-each pen (log-marginal->marginal (time (marginalize query-nocache-expr))))
