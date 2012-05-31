#!r6rs

(import (rnrs)
        (scheme-tools)
        (sine env-lexical))

(pretty-print
 (extend-environment
  (list 'a 'b)
  (list 1 2)
  the-empty-environment))

