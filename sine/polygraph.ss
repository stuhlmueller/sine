#!r6rs

(library

 (sine polygraph)

 (export interpreter-thunk->polygraph)

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
         (scheme-tools queue)
         (sine polycommon)
         (sine coroutine-interpreter)
         (sine coroutine-id)
         (sine delimcc-simple-r6rs))

 (define (interpreter-thunk->polygraph thunk)
   (let ([graph (make-graph)]
         [queue (make-queue (make-task thunk 'root 0.0 #t))])
     (graph:add-node! graph 'root)
     (graph:set-root! graph 'root)
     (build-graph graph queue)))

 (define (build-graph graph queue)
   (let loop ()
     (if (queue-empty? queue)
         graph
         (let ([task (dequeue! queue)])
           (when (not (void? task))
                 (let ([node ((task->thunk task))])
                   (cond [(void? node) #f]
                         [(xrp? node) (build-graph:xrp! graph queue task node)]
                         [(recur? node) (build-graph:recur! graph queue task node)]
                         [(terminal? node) (build-graph:terminal! graph queue task node)]
                         [else (error node "build-graph: unknown obj type")])))
           (loop)))))

 (define (build-graph:xrp! graph queue task xrp)
   (for-each (lambda (value score)
               (enqueue! queue
                         (make-task (lambda () ((xrp-cont xrp) value))
                                    (task->last-id task)
                                    (plus/symbolic (task->link-weight task) score)
                                    (cons value (task->link-label task)))))
             (xrp-vals xrp)
             (xrp-probs xrp)))

 (define (build-graph:terminal! graph queue task terminal)
   (let ([terminal-id (terminal-id terminal)])
     (graph:add/link! graph
                      (task->last-id task)
                      terminal-id
                      (task->link-label task)
                      (task->link-weight task))
     (enqueue! queue
               (make-task (lambda () (for-each (lambda (terminal->task)
                                            (enqueue! queue (terminal->task terminal-id)))
                                          (graph:ancestor-callbacks graph (task->last-id task))))))))

 (define (build-graph:recur! graph queue task recur)
   (let* ([subthunk (lambda () (reset (make-terminal
                                  (apply (recur-call recur)
                                         (recur-state recur)))))]
          [subroot-id (recur-id recur)]
          [subroot-is-new (graph:add/retrieve! graph subroot-id)]
          [seen? (make-watcher)]
          [terminal->task
           (lambda (terminal-id)
             (when (not (seen? terminal-id))
                   (let ([value (terminal-id->value terminal-id)])
                     (make-task
                      (lambda () ((recur-cont recur) value))
                      (task->last-id task)
                      (plus/symbolic (make-score-ref subroot-id terminal-id)
                                     (task->link-weight task))
                      (cons value (task->link-label task))))))])
     (graph:register-callback! graph subroot-id terminal->task)
     (enqueue! queue
               (if subroot-is-new
                   (make-task subthunk subroot-id 0.0 #t)
                   (make-task (lambda ()
                                (for-each (lambda (terminal-id) (enqueue! queue (terminal->task terminal-id)))
                                          (graph:reachable-terminals graph subroot-id))))))))

 )