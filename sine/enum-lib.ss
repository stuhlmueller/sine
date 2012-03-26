#!r6rs

(library

 (sine enum-lib)

 (export marginalize flip)

 (import (rnrs)
         (scheme-tools)
         (sine shift-reset-enumerator))

 (define source (make-parameter #f))
 
 (define (flip . ?p)
   (let ([p (if (null? ?p) .5 (car ?p))])
     ((source) p)))

 (define (marginalize thunk)
   (enumerate (lambda (enum-source)
                (parameterize ([source enum-source])
                              (thunk)))))

 )