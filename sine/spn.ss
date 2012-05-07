#!r6rs

;; Todo:
;; - write remainder of algorithm that solves spn
;;   spn->equations
;;   solve-equations using toposort
;; - extract spn info into data structured that can be returned in its
;;   entirety from build-spn
;; - make interpreter state hashing incremental
;;
;; Analysis:
;; - show that it is impossible to call a callback more than once per value
;; - show that we don't duplicate edges
;; - show that message passing does not change asymptotic complexity
;; - does merging at terminals matter?

;; Notes:
;; - Object hashing takes place whenever we use terminal-id and recur-id
;; - the dupe check in get-terminal-ids (used in build-spn:recur!
;;   via store-callback) is linear in the number of terms

(library

 (sine spn)

 (export build-spn)

 (import (rnrs)
         (scheme-tools srfi-compat :1)
         (scheme-tools queue)
         (only (scheme-tools) gensym)
         (sine coroutine-interpreter)
         (sine coroutine-id)
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
   (define spn:indicator-ids (make-eq-hashtable))
   (define spn:ref-terminal-ids (make-eq-hashtable))
   (define spn:ref-subroot-ids (make-eq-hashtable))
   (define spn:probs (make-eq-hashtable))
   (define spn:roots (make-eq-hashtable))
   (define spn:callbacks (make-eq-hashtable))
   (define spn:terminal-ids (make-eq-hashtable))
   (define spn:recur-id (make-eq-hashtable))
   (define spn:terminal-id (make-eq-hashtable))

   (define (object-id/tracked get-obj-id get-obj-state spn:obj-id obj obj-type)
     (let* ([obj-id (get-obj-id obj)]
            [spn-id (hashtable-ref spn:obj-id obj-id #f)])
       (if spn-id
           (values spn-id #f)
           (let ([id (gensym obj-type)])
             (hashtable-set! spn:obj-id (get-obj-state obj) id)
             (values id #t)))))

   (define (recur-id/tracked recur)
     (object-id/tracked recur-id recur-state spn:recur-id recur 'subroot))

   (define (get-root-id id)
     (let* ([err (gensym)]
            [root-id (hashtable-ref spn:roots id err)])
       (assert (not (eq? err root-id)))
       root-id))

   (define (store-root-id! id root-id)
     (hashtable-set! spn:roots id root-id))

   (define (get-callbacks root-id)
     (hashtable-ref spn:callbacks root-id '()))

   (define (store-callback! source-id source-cont subroot-id)
     (let* ([old-callbacks (get-callbacks subroot-id)]
            [updated-callbacks (cons (make-callback source-id source-cont) old-callbacks)]
            [enq! (lambda (t) (enqueue! queue (make-task source-id (lambda () (source-cont t)))))])
       (hashtable-set! spn:callbacks subroot-id updated-callbacks)
       (for-each enq! (get-terminals subroot-id))))

   (define (cons-if-new obj lst eql?)
     (if (any (lambda (x) (eql? x obj)) lst)
         (values lst #f)
         (values (cons obj lst) #t)))

   ;; LINEAR IN THE NUMBER OF TERMINALS
   (define (store-terminal-id! root-id terminal-id)
     (let-values ([(new-terminal-ids id-is-new)
                   (cons-if-new terminal-id
                                (get-terminal-ids root-id)
                                eq?)])
       (hashtable-set! spn:terminal-ids
                       root-id
                       new-terminal-ids)
       id-is-new))

   (define (get-terminal-ids root-id)
     (hashtable-ref spn:terminal-ids root-id '()))

   (define (get-terminals root-id)
     (map terminal-id->value (get-terminal-ids root-id)))

   (define (make-edge! from to)
     (hashtable-set! spn:edges
                     from
                     (cons to (hashtable-ref spn:edges from '()))))

   (define (make-spn-node! parent-id node-type)
     (let ([node-id (gensym node-type)])
       (make-edge! parent-id node-id)
       (hashtable-set! spn:nodetypes node-id node-type)
       (store-root-id! node-id (get-root-id parent-id))
       node-id))

   (define (make-sum-node! parent-id)
     (make-spn-node! parent-id 'sum))

   (define (make-product-node! parent-id)
     (make-spn-node! parent-id 'product))

   (define (make-indicator-node! parent-id terminal-id)
     (let ([ind-id (make-spn-node! parent-id 'indicator)])
       (hashtable-set! spn:indicator-ids ind-id terminal-id)))

   (define (make-ref-node! parent-id subroot-id terminal-id)
     (let ([ref-id (make-spn-node! parent-id 'ref)])
       (hashtable-set! spn:ref-terminal-ids ref-id terminal-id)
       (hashtable-set! spn:ref-subroot-ids ref-id subroot-id)
       ref-id))

   (define (make-prob-node! parent-id prob)
     (let ([prob-id (make-spn-node! parent-id 'prob)])
       (hashtable-set! spn:probs prob-id prob)
       prob-id))

   (define (make-root-node! id)
     (store-root-id! id id)
     (hashtable-set! spn:nodetypes id 'root))


   ;; Queue

   (define queue (make-empty-queue))

   (define (make-task last-id thunk)
     (lambda ()
       (let ([val (thunk)])
         (cond [(xrp? val) (build-spn:xrp! last-id val)]
               [(recur? val) (build-spn:recur! last-id val)]
               [(terminal? val) (build-spn:terminal! last-id val)]
               [else (error val "build-spn: unknown object type")]))))


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
     (let* ([root-id (get-root-id last-id)]
            [term-id (terminal-id terminal)]
            [enq! (lambda (cb)
                    (let ([product-id (make-product-node! (callback->source-id cb))])
                      (make-ref-node! product-id root-id term-id)
                      (enqueue! queue
                                (make-task product-id
                                           (lambda () ((callback->source-cont cb)
                                                       (terminal-value terminal)))))))]
            [term-is-new (store-terminal-id! root-id term-id)])
       (if term-is-new
           (begin
             (make-indicator-node! last-id term-id)
             (for-each enq! (get-callbacks root-id)))
           (make-edge! last-id term-id))))

   (define (build-spn:recur! last-id recur)
     (let-values ([(sum-id) (make-sum-node! last-id)]
                  [(subroot-id subroot-new) (recur-id/tracked recur)])
       (when subroot-new
             (make-root-node! subroot-id)
             (enqueue! queue (make-task subroot-id (make-subthunk recur))))
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
     spn:edges)

   (main))))

)