#!r6rs

;; A fake version of the value number module that doesn't convert to
;; value numbers (for profiling).

(library

 (sine value-number-noop)

 (export &cadddr
         &caddr
         &cadr
         &car
         &cddddr
         &cdddr
         &cddr
         &cdr
         &cons
         &eq?
         &expand-boolean
         &expand-list
         &expand-null
         &expand-number
         &expand-pair
         &expand-recursive
         &expand-symbol
         &expand-vector
         &expand-procedure
         &id
         &list
         &list-ref
         &null?
         &pair?
         &symbol?
         &tagged-list?
         &vector
         &vector-ref
         &vector-append
         &vector-length
         &vector-index
         &vector?
         compress-boolean
         compress-list
         compress-null
         compress-number
         compress-pair
         compress-recursive
         compress-symbol
         compress-vector
         compress-procedure)

 (import (rnrs)
         (scheme-tools)
         (scheme-tools srfi-compat :43))

 (define &cadddr cadddr)
 (define &caddr caddr)
 (define &cadr cadr)
 (define &car car)
 (define &cddddr cddddr)
 (define &cdddr cdddr)
 (define &cddr cddr)
 (define &cdr cdr)
 (define &cons cons)
 (define &eq? eq?)
 (define &expand-boolean identity)
 (define &expand-list identity)
 (define &expand-null identity)
 (define &expand-number identity)
 (define &expand-pair identity)
 (define &expand-recursive identity)
 (define &expand-symbol identity)
 (define &expand-vector identity)
 (define &expand-procedure identity)
 (define &id identity)
 (define &list list)
 (define &list-ref list-ref)
 (define &null? null?)
 (define &pair? pair?)
 (define &symbol? symbol?)
 (define &tagged-list? tagged-list?)
 (define &vector vector)
 (define &vector-ref vector-ref)
 (define &vector-append vector-append)
 (define &vector-length vector-length)
 (define &vector-index vector-index)
 (define &vector? vector?)
 (define compress-boolean identity)
 (define compress-list identity)
 (define compress-null identity)
 (define compress-number identity)
 (define compress-pair identity)
 (define compress-recursive identity)
 (define compress-symbol identity)
 (define compress-vector identity)
 (define (compress-procedure proc name) proc)

 )