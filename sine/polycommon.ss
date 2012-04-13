#!r6rs

;; Utilities used by the polynomial graph-building and
;; equation-generating part of cosh.

(library

 (sine polycommon)

 (export graph:reachable-terminals
         graph:notify-ancestors-of-connection!
         score-ref->terminal-node
         score-ref->root
         score-ref?
         make-score-ref
         list-of-weights
         plus/symbolic
         make-task
         task->thunk
         task->last-id
         task->link-weight
         task->link-label)

 (import (rnrs)
         (cosh continuation)
         (cosh application)
         (scheme-tools)
         (scheme-tools object-id)
         (scheme-tools hash)
         (scheme-tools mem)
         (scheme-tools watcher)
         (scheme-tools graph)
         (scheme-tools graph callback)
         (scheme-tools graph utils)
         (scheme-tools srfi-compat :1)
         (sine coroutine-interpreter)
         (sine coroutine-id))

 (define (graph:reachable-terminals graph node)
   (traverse node
             (lambda (node) (graph:children graph node))
             (lambda (node list-of-terminals)
               (if (terminal-id? node)
                   (cons node list-of-terminals)
                   (apply append list-of-terminals)))
             (make-watcher)
             '()))

 (define (graph:terminals&callbacks graph node last-node)
   (let ([terminals (graph:reachable-terminals graph node)]
         [callbacks (graph:ancestor-callbacks graph last-node)])
     (values terminals callbacks)))

 (define (graph:notify-ancestors-of-connection! graph node last-node)
   (let-values ([(terminals callbacks) (graph:terminals&callbacks graph node last-node)])
     (map (lambda (callback) (map (lambda (terminal) (callback terminal)) terminals))
          callbacks)))

 (define (make-score-ref root-node terminal-node)
   (list 'score-ref root-node terminal-node))

 (define (score-ref? obj)
   (tagged-list? obj 'score-ref))

 (define score-ref->root second)

 (define score-ref->terminal-node third)

 (define (list-of-weights weight)
   (cond [(score-ref? weight) (list weight)]
         [(number? weight) (list weight)]
         [(and (list? weight) (not (null? weight)) (eq? (car weight) '+)) (cdr weight)]
         [else (error weight "unknown weight type")]))

 (define (plus/symbolic w1 w2)
   `(+ ,@(list-of-weights w1)
       ,@(list-of-weights w2)))

 (define (make-task thunk . args)
   (append (list 'task thunk) args))

 (define task->thunk second)

 (define task->last-id third)

 (define task->link-weight fourth)

 (define task->link-label fifth)

 )
