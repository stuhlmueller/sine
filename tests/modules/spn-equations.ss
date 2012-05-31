#!r6rs

(import (rnrs)
        (scheme-tools)
        (sine spn-equations))

(define equations
  '((= x 1)
    (= y x)
    (= z (+ y 2))))

(for-each pretty-print
          (simplify-equations equations))
