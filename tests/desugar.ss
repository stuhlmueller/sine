#!r6rs

(import (rnrs)
        (scheme-tools)
        (sine desugar))

(pretty-print
 (desugar-all
  '(begin
     (define x 1)
     (define (f x) (* x x))
     (let* ([y 2]
            [z 3])
       (f (+ x y))))))


