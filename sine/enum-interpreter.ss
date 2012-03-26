#!r6rs

;; Enumerate by controlling the random source of the interpreter.

(library

 (sine enum-interpreter)

 (export enum-interpreter)

 (import (rnrs)
         (sine shift-reset-enumerator)
         (sine interpreter)
         (scheme-tools)) 

 (define (enum-interpreter expr)
   (enumerate (lambda (source)
                (interpreter expr
                             (make-default-recur source)
                             source))))

 )