#!r6rs

(import (rnrs)
        (scheme-tools)
        (sine syntax))

(pretty-print
 (sexpr->syntax '(let* ([x 1]
                        [y 2])
                   (+ x y))))

