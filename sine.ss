#!r6rs

(library

 (sine)

 (export marginalize flip)

 (import (rnrs)
         (except (scheme-tools) flip)
         (scheme-tools srfi-compat :1)
         (scheme-tools hash)
         (delimcc-simple-r6rs))

 (define-record-type xrp-cont
   (fields id proc vals probs)
   (protocol
    (lambda (n)
      (lambda (c vs ps) 
        (n (gensym) c vs ps)))))

 (define (get-id obj)
   (if (xrp-cont? obj)
       (xrp-cont-id obj)
       obj))

 (define (flip)
   (shift f (handler (make-xrp-cont f '(#t #f) '(.5 .5)))))

 (define root #f)

 (define edge-table (make-eq-hashtable))

 (define (store-edge! from to p)
   (hashtable-set! edge-table
                   (get-id from)
                   (cons (pair (get-id to) p)
                         (hashtable-ref edge-table
                                        (get-id from)
                                        '()))))

 (define (handler obj)
   (when (eq? root #f)
         (set! root obj))
   (when (xrp-cont? obj)
         (for-each (lambda (v p) (store-edge! obj ((xrp-cont-proc obj) v) p))
                   (xrp-cont-vals obj)
                   (xrp-cont-probs obj)))
   obj)

 (define (edge-table->marginals table root p)
   (let ([marginals (make-equal-hash-table)])
     (let loop ([root root]
                [p p])
       (let ([children (hashtable-ref table root '())])
         (if (null? children)
             (hash-table-set! marginals
                              root
                              (+ p (hash-table-ref/default marginals
                                                           root
                                                           0.0)))
             (for-each (lambda (child) (loop (car child)
                                        (* p (cdr child))))
                       children))))
     marginals))

 (define (marginalize thunk)
   (set! root #f)
   (set! edge-table (make-eq-hashtable))
   (reset (thunk))
   (hash-table->alist (edge-table->marginals edge-table (get-id root) 1.0)))
 

 )