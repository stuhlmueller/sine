#!r6rs

(import (rnrs)
        (sine)
        (only (scheme-tools) pretty-print))

(define (test-program)
  (or (flip) (flip)))

(for-each pretty-print (marginalize test-program))
