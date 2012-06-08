#!r6rs

(library

 (sine build-spn)

 (export build-spn
         spn->edges
         spn->nodetypes
         spn->indicator-ids
         spn->ref-terminal-ids
         spn->ref-subroot-ids
         spn->probs
         spn->roots
         spn->callbacks
         spn->terminal-ids
         spn->recur-id
         spn->terminal-id)

 (import (rnrs)
         (scheme-tools srfi-compat :1)
         (scheme-tools srfi-compat :43)
         (scheme-tools queue)
         (scheme-tools debug)
         (scheme-tools math)
         (scheme-tools)
         (sine utils)
         (sine hashtable)
         (only (scheme-tools) gensym)
         (sine coroutine-interpreter)
         (sine coroutine-id)
         (sine value-number)
         (sine syntax)
         (sine delimcc-simple-r6rs))

 (define counter
   (get-counter))

 (define (readable-gensym sym)
   (sym+num (sym-append sym '-) (counter)))

 (define (make-subthunk recur)
   (lambda () (reset (make-terminal
                      (apply-recur recur)))))

 (define (make-callback source-id source-cont)
   (list 'callback source-id source-cont))

 (define callback->source-id second)

 (define callback->source-cont third)

 (define (make-spn-bundle . args)
   (cons 'spn (cons 'dummy args)))

 (define (spn->edges bundle) (list-ref bundle 2))
 (define (spn->nodetypes bundle) (list-ref bundle 3))
 (define (spn->indicator-ids bundle) (list-ref bundle 4))
 (define (spn->ref-terminal-ids bundle) (list-ref bundle 5))
 (define (spn->ref-subroot-ids bundle) (list-ref bundle 6))
 (define (spn->probs bundle) (list-ref bundle 7))
 (define (spn->roots bundle) (list-ref bundle 8))
 (define (spn->callbacks bundle) (list-ref bundle 9))
 (define (spn->terminal-ids bundle) (list-ref bundle 10))
 (define (spn->recur-id bundle) (list-ref bundle 11))
 (define (spn->terminal-id bundle) (list-ref bundle 12))


 (define/kw (build-spn root-thunk [max-spn-size :default +inf.0])

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
   (define spn:size 0)

   (define (recur-id/new? recur)
     (let* ([spn-id (hashtable-ref spn:recur-id (recur-id recur) #f)])
       (if spn-id
           (values (recur-id recur) #f)

           (begin
             (hashtable-set! spn:recur-id (recur-id recur) (recur-id recur))
             (values (recur-id recur) #t))

           ;; (let ([new-id (gensym)])
           ;;   (hashtable-set! spn:recur-id new-id (recur-id recur))
           ;;   (values new-id #t))

           )))

   (define (get-root-id id)
     (let* ([err (gensym)]
            [root-id (hashtable-ref spn:roots id err)])
       (assert (not (eq? err root-id)))
       root-id))

   (define (store-root-id! id root-id)
     (hashtable-set! spn:roots id root-id))

   (define (get-callbacks root-id)
     (hashtable-ref spn:callbacks root-id '()))

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
       (when id-is-new
             (hashtable-set! spn:terminal-ids
                             root-id
                             new-terminal-ids))
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
     (let ([node-id (readable-gensym node-type)])
       ;; (pe "make-spn-node! " parent-id " -> " node-id "\n")
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


   ;; Algorithm

   (define queue (make-empty-queue))

   (define (make-task last-id thunk)
     (lambda ()
       (if (> spn:size max-spn-size)
           (make-prob-node! last-id LOG-PROB-0)
           (let ([val (thunk)])
             (set! spn:size (+ spn:size 1))
             (cond [(xrp? val) (build-spn:xrp! last-id val)]
                   [(recur? val) (build-spn:recur! last-id val)]
                   [(terminal? val) (build-spn:terminal! last-id val)]
                   [else (error val "build-spn: unknown object type")])))))

   (define (build-spn:xrp! last-id xrp)
     ;; (pe "build-spn:xrp! " last-id " " (xrp-probs xrp) "\n")
     (let ([sum-id (make-sum-node! last-id)])
       (vector-for-each (lambda (value score)
                          (let ([product-id (make-product-node! sum-id)])
                            (make-prob-node! product-id score)
                            (enqueue! queue
                                      (make-task product-id (lambda () ((xrp-cont xrp) value))))))
                        (xrp-vals xrp)
                        (xrp-probs xrp))))

   (define (process-terminal root-id term-id callback)
     (let ([product-id (make-product-node! (callback->source-id callback))])
       (make-ref-node! product-id root-id term-id)
       (enqueue! queue
                 (make-task product-id
                            (lambda () ((callback->source-cont callback)
                                        (terminal-id->value term-id)))))))

   (define (build-spn:terminal! last-id terminal)
     ;; (pe "build-spn:terminal! " last-id " " (&->string:n (terminal-value terminal) 30) "\n")
     (let* ([root-id (get-root-id last-id)]
            [term-id (terminal-id terminal)]
            [term-is-new (store-terminal-id! root-id term-id)])
       (make-indicator-node! last-id term-id)
       (when term-is-new
             (for-each (lambda (callback) (process-terminal root-id (terminal-id terminal) callback))
                       (get-callbacks root-id)))))

   (define (store-callback! source-id source-cont subroot-id)
     (let* ([old-callbacks (get-callbacks subroot-id)]
            [new-callback (make-callback source-id source-cont)]
            [updated-callbacks (cons new-callback old-callbacks)])
       (hashtable-set! spn:callbacks subroot-id updated-callbacks)
       new-callback))

   (define (build-spn:recur! last-id recur)
     (let-values ([(sum-id) (make-sum-node! last-id)]
                  [(subroot-id subroot-new) (recur-id/new? recur)])
       (let ([new-callback (store-callback! sum-id
                                            (lambda (t) ((recur-cont recur) t))
                                            subroot-id)])
         (if subroot-new
             (begin
               (make-root-node! subroot-id)
               (enqueue! queue (make-task subroot-id (make-subthunk recur))))
             (for-each (lambda (term-id)
                         (process-terminal subroot-id term-id new-callback))
                       (get-terminal-ids subroot-id))))))

   (define (process-queue!)
     (when (not (queue-empty? queue))
           (let ([task (dequeue! queue)])
             (task)
             (process-queue!))))

   (define (main)
     (make-root-node! 'root)
     (enqueue! queue (make-task 'root root-thunk))
     (process-queue!)
     (make-spn-bundle spn:edges
                      spn:nodetypes
                      spn:indicator-ids
                      spn:ref-terminal-ids
                      spn:ref-subroot-ids
                      spn:probs
                      spn:roots
                      spn:callbacks
                      spn:terminal-ids
                      spn:recur-id
                      spn:terminal-id))

   (main))))

)