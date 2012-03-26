#!r6rs

(import (rnrs)
        (scheme-tools)
        (sine enum-lib))

(define (test)
  (if (flip)
      (+ 1 (if (flip) 3 4))
      4))

(for-each pretty-print
          (marginalize test))


