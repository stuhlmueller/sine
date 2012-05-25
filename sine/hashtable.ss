#!r6rs

(library

 (sine hashtable)

 (export hashtable-ref/default
         hashtable-keys
         hashtable-values
         pretty-print-hashtable)

 (import (rnrs)
         (only (scheme-tools) gensym pretty-print))

 (define not-found (gensym 'not-found))

 (define (hashtable-ref/default table key thunk)
   (let ([v (hashtable-ref table key not-found)])
     (if (eq? v not-found)
         (let ([v* (thunk)])
           (hashtable-set! table key v*)
           v*)
         v)))

 (define (hashtable-values ht)
   (let-values ([(keys vals) (hashtable-entries ht)])
     vals))

 (define (pretty-print-hashtable ht)
   (let-values ([(keys vals) (hashtable-entries ht)])
     (for-each (lambda (k v) (pretty-print (cons k v)))
               (vector->list keys)
               (vector->list vals))))

 )