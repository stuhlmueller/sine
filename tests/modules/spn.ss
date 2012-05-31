#!r6rs

(import (rnrs)
        (scheme-tools)
        (sine spn))

(marginalize '(list ((lambda (x) (not x)) (flip .1)) (flip .5)))

