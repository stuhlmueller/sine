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

(define nocache-recursion-expr
  '(begin
     (define foo
       (lambda ()
         (if (flip)
             (not (foo))
             true)))
     (foo)))

(define cache-recursion-expr
  '(begin
     (define foo
       (lambda ()
         (cache
          (if (flip)
              (not (foo))
              true))))
     (foo)))

(pen "no self-recursion, no cache:")
(for-each pen (log-marginal->marginal (time (marginalize nocache-expr))))

(pen "no self-recursion, cache:")
(for-each pen (log-marginal->marginal (time (marginalize cache-expr))))

(pen "self-recursion, no cache:")
(for-each pen (log-marginal->marginal (time (marginalize nocache-recursion-expr 'max-spn-size 100))))

(pen "self-recursion, cache:")
(for-each pen (log-marginal->marginal (time (marginalize cache-recursion-expr))))
