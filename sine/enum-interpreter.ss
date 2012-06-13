#!r6rs

;; Enumerate by controlling the random source of the interpreter.

(library

 (sine enum-interpreter)

 (export enum-interpreter)

 (import (rnrs)
         (scheme-tools value-number)
         (scheme-tools)
         (sine interpreter)
         (sine preamble)
         (sine shift-reset-enumerator))

 (define/kw (enum-interpreter expr [limit :default 10000])
   (let ([marginals
          (enumerate (lambda (source)
                       (interpreter (with-preamble expr)
                                    (make-default-recur source)))
                     'limit limit)])
     (map (lambda (binding)
            (pair (&expand-recursive (car binding))
                  (cdr binding)))
          marginals)))

 )