#!r6rs

(import (rnrs)
        (scheme-tools)
        (sine desugar))

(pretty-print
 (desugar-all
  '(let* ([x 1]
          [y 2])
     (+ x y))))


