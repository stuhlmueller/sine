#!r6rs

;; TODO:
;; - store root node for each spn node
;; - extract mapping from values to ids (for terminals, interpreter state)
;; - make sure that we enqueue each callback only once per value
;;   - we're not merging terminals in build-spn:terminal! (against current implicit semantics)
;;   - if we notice previously seen terminals for a subroot in build-spn:terminal!, do we not
;;     need to check that ecah callback is only called once?
;; - extract into spn data structured that can be returned in its entirety from build-spn
;; - make interpreter state hashing incremental

(library

 (sine spn)

 (export build-spn)

 (import (rnrs)
         (scheme-tools queue)
         (sine delimcc-simple-r6rs))

 (define (make-subthunk recur)
   (lambda () (reset (make-terminal
                 (apply (recur-call recur)
                        (recur-state recur))))))

 (define (make-callback source-id source-cont)
   (list 'callback source-id source-cont))

 (define callback->source-id second)

 (define callback->source-cont third)


 (define (build-spn root-thunk)

   ;; SPN data structure

   (define spn:edges (make-eq-hashtable))
   (define spn:nodetypes (make-eq-hashtable))
   (define spn:indicator-vals (make-eq-hashtable))
   (define spn:ref-vals (make-eq-hashtable))
   (define spn:ref-subroot-ids (make-eq-hashtable))
   (define spn:probs (make-eq-hashtable))
   (define spn:roots (make-eq-hashtable))
   (define spn:callbacks (make-eq-hashtable))
   (define spn:terminals (make-eq-hashtable))

   (define (get-root-id id)
     (hash-table-ref spn:roots id))

   (define (store-root-id! id root-id)
     (hashtable-set! spn:roots id root-id))

   (define (get-callbacks root-id)
     (hashtable-ref/default spn:callbacks root-id '()))

   (define (store-callback! source-id source-cont subroot-id)
     (let* ([old-callbacks (get-callbacks subroot-id)]
            [updated-callbacks (cons (make-callback source-id source-cont) old-callbacks)]
            [enq! (lambda (t) (enqueue! queue (make-task source-id (lambda () (source-cont )t))))])
       (hash-table-set! spn:callbacks subroot-id updated-callbacks)
       (for-each enq! (get-terminals subroot-it))))

   (define (store-terminal! root-id terminal)
     (hash-table-set! spn:terminals
                      root-id
                      (cons terminal (get-terminals root-id))))

   (define (get-terminals root-id)
     (hash-table-ref/default spn:terminals root-id '()))

   (define (make-edge! from to)
     (hashtable-set! spn:edges
                     from
                     (cons to (hashtable-get spn:edges from '()))))

   (define (make-spn-node! parent-id node-type)
     (let ([node-id (gensym node-type)])
       (make-edge! parent-id node-id)
       (hashtable-set! spn:nodetypes node-id node-type)
       node-id))

   (define (make-sum-node! parent-id)
     (make-spn-node! parent-id 'sum))

   (define (make-product-node! parent-id)
     (make-spn-node! parent-id 'product))

   (define (make-indicator-node! parent-id terminal)
     (let ([ind-id (make-spn-node! parent-id 'indicator)])
       (hashtable-set spn:indicator-vals ind-id terminal)))

   (define (make-ref-node! parent-id subroot-id terminal)
     (let ([ref-id (make-spn-node! parent-id 'ref)])
       (hashtable-set spn:ref-vals ref-id terminal)
       (hashtable-set spn:ref-subroot-ids ref-id subroot-id)
       ref-id))

   (define (make-prob-node! parent-id prob)
     (let ([prob-id (make-spn-node! parent-id 'prob)])
       (hashtable-set spn:probs prob-id prob)
       prob-id))

   (define (make-root-node! id)
     (hashtable-set! spn:nodetypes id 'root))


   ;; Queue

   (define queue (make-empty-queue))

   (define (make-task last-id thunk)
     (lambda ()
       (let ([val (thunk)])
         (cond [(xrp? val) (build-spn:xrp! last-id val)]
               [(recur? val) (build-spn:recur! last-id val)]
               [(terminal? val) (build-spn:terminal! last-id val)]
               [else (error val "unknown object type")]))))


   ;; Algorithm

   (define (build-spn:xrp! last-id xrp)
     (let ([sum-id (make-sum-node! last-id)])
       (for-each (lambda (value score)
                   (let ([product-id (make-product-node! sum-id)])
                     (make-prob-node! product-id score)
                     (enqueue! queue
                               (make-task product-id (lambda () ((xrp-cont xrp) value))))))
                 (xrp-vals xrp)
                 (xrp-probs xrp))))

   (define (build-spn:terminal! last-id terminal)
     (let ([root-id (get-root-id last-id)]
           [enq! (lambda (cb)
                   (let ([product-id (make-product-node! (callback->source-id cb))])
                     (make-ref-node! product-id root-id terminal)
                     (enqueue! queue
                               (make-task product-id
                                          (lambda () ((callback->source-cont cb) terminal))))))])
       (make-indicator-node! last-id terminal)
       (store-terminal! root-id terminal)
       (for-each enq! (get-callbacks root-id))))

   (define (build-spn:recur! last-id recur)
     (let ([sum-id (make-sum-node! last-id)]
           [subroot-id
            (hash-table-ref state->id
                            (recur-state recur)
                            (lambda ()
                              (let ([id (gensym)])
                                (make-root-node! id)
                                (hash-table-set! state->id (recur-state recur) id)
                                (enqueue! queue (make-task id (make-subthunk recur)))
                                id)))])
       (store-callback! sum-id (lambda (t) ((recur-cont recur) t)) subroot-id)))

   (define (process-queue!)
     (when (not (queue-empty? queue))
           (let ([task (dequeue! queue)])
             (task)
             (process-queue!))))

   (define (main)
     (make-root-node! 'root)
     (enqueue! queue (make-task 'root root-thunk))
     (process-queue!)
     spn)

   (main))))

)