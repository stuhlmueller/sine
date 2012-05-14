#!r6rs

(library

 (sine preamble)

 (export with-preamble)

 (import (rnrs))

 (define (with-preamble expr)
   `(begin

      (define (Y f)
        (let ([g (lambda (g)
                   (f (lambda args
                        (apply (g g) args))))])
          (g g)))

      ,expr

      ))

 )