#!r6rs

(import (rnrs)
        (sine hashtable)
        (sine spn-solve)
        (scheme-tools))

(define equations
  '((= y 0.1)
    (= x (+ y .1))
    (= z (+ y .2))))

(pretty-print (hashtable->alist (solve-equations equations)))