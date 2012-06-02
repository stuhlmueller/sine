#!r6rs

(import (rnrs)
        (scheme-tools)
        (sine utils)
        (sine spn))

(define nocache-expr
  '(begin
     (define (foo)
       (all (list (flip) (flip) (flip))))
     (and (foo) (foo) (foo))))

(define cache-expr
  '(begin
     (define (foo)
       (cache (all (list (flip) (flip) (flip)))))
     (and (foo) (foo) (foo))))

(pen "no cache:")
(for-each pen (log-marginal->marginal (time (marginalize nocache-expr))))

(pen "cache:")
(for-each pen (log-marginal->marginal (time (marginalize cache-expr))))
