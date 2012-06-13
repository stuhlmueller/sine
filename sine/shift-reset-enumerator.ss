#!r6rs

;; Use delimited continuations to compute marginals by enumeration for
;; any procedure that is parameterized by its source of (Boolean)
;; randomness).

(library

 (sine shift-reset-enumerator)

 (export enumerate)

 (import (rnrs)
         (scheme-tools hash)
         (scheme-tools math)
         (scheme-tools queue)
         (scheme-tools srfi-compat :1)
         (scheme-tools srfi-compat :43)
         (scheme-tools value-number)
         (scheme-tools)
         (sine delimcc-simple-r6rs))


 ;; --------------------------------------------------------------------
 ;; xrp-cont data structure

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


 ;; --------------------------------------------------------------------
 ;; Globals

 (define root #f)

 (define edge-table (make-eq-hashtable))

 (define xrp-count 0)

 (define xrp-count-limit 10000)

 (define queue (make-empty-queue))


 ;; --------------------------------------------------------------------
 ;; Algorithm

 (define (handler obj)
   (when (eq? root #f)
         (set! root obj))
   (when (and (xrp-cont? obj)
              (not (> xrp-count xrp-count-limit)))
         (set! xrp-count (+ xrp-count 1))
         (vector-for-each
          (lambda (v p) (enqueue! queue (lambda () (store-edge! obj ((xrp-cont-proc obj) v) p))))
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

 (define (marginal-value? value)
   (not (equal? (&expand-recursive value) value)))

 (define (edge-table->marginals table root p)
   (let ([marginals (make-equal-hash-table)])
     (let loop ([root root]
                [p p])
       (let ([children (hashtable-ref table root '())])
         (if (and (null? children)
                  (marginal-value? root))
             (hash-table-set! marginals
                              root
                              (logsumexp p
                                         (hash-table-ref/default marginals
                                                                 root
                                                                 LOG-PROB-0)))
             (for-each (lambda (child) (loop (car child)
                                             (+ p (cdr child))))
                       children))))
     marginals))

 (define (enumeration-source vs logps)
   (shift f (handler (make-xrp-cont f
                                    vs
                                    (&expand-recursive logps)))))

 (define (process-queue!)
   (if (queue-empty? queue)
       'done
       (begin
         ((dequeue! queue))
         (process-queue!))))

 (define/kw (enumerate proc [limit :default 10000])
   (set! root #f)
   (set! edge-table (make-eq-hashtable))
   (set! xrp-count-limit limit)
   (enqueue! queue (lambda () (reset (proc enumeration-source))))
   (process-queue!)
   (hash-table->alist (edge-table->marginals edge-table (get-id root) LOG-PROB-1)))

 )