#!r6rs

(import (rnrs)
        (sine)
        (only (scheme-tools) pretty-print))

(define (test)
  (if (flip)
      (+ 1 (if (flip) 3 4))
      4))

(for-each pretty-print (marginalize test))
