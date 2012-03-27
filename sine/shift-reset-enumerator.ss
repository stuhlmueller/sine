#!r6rs

;; Use delimited continuations to compute marginals by enumeration for
;; any procedure that is parameterized by its source of (Boolean)
;; randomness).

(library

 (sine shift-reset-enumerator)

 (export enumerate)

 (import (rnrs)
         (sine delimcc-simple-r6rs)
         (scheme-tools)
         (scheme-tools hash)
         (scheme-tools srfi-compat :1))

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

 (define root #f)

 (define edge-table (make-eq-hashtable))

 (define (handler obj)
   (when (eq? root #f)
         (set! root obj))
   (when (xrp-cont? obj)
         (for-each (lambda (v p) (store-edge! obj ((xrp-cont-proc obj) v) p))
                   (xrp-cont-vals obj)
                   (xrp-cont-probs obj)))
   obj)

 (define (store-edge! from to p)
   (hashtable-set! edge-table
                   (get-id from)
                   (cons (pair (get-id to) p)
                         (hashtable-ref edge-table
                                        (get-id from)
                                        '()))))

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

 (define (enumeration-source p)
   (shift f (handler (make-xrp-cont f '(#t #f) (list p (- 1 p))))))

 (define (enumerate proc)
   (set! root #f)
   (set! edge-table (make-eq-hashtable))
   (reset (proc enumeration-source))
   (hash-table->alist (edge-table->marginals edge-table (get-id root) 1.0)))

 )