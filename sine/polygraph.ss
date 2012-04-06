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
         (sine polycommon)
         (sine coroutine-interpreter)
         (sine coroutine-id)
         (sine delimcc-simple-r6rs))

 (define (interpreter-thunk->polygraph thunk)
   (let ([graph (make-graph)]
         [root-link-promise (make-link-promise 0.0 #t)])
     (graph:add-node! graph 'root)
     (graph:set-root! graph 'root)
     (build-graph graph thunk 'root root-link-promise)
     graph))

 (define (build-graph graph thunk last-id link-promise)
   (let ([node (thunk)])
     (cond [(xrp? node) (build-graph:xrp graph node last-id link-promise)]
           [(recur? node) (build-graph:recur graph node last-id link-promise)]
           [(terminal? node) (build-graph:terminal graph node last-id link-promise)]
           [else (error node "build-graph: unknown obj type")])))

 (define (build-graph:xrp graph xrp last-id link-promise)
   (for-each (lambda (value score)
               (build-graph graph
                            (lambda () ((xrp-cont xrp) value))
                            last-id
                            (multiply-link-promises
                             link-promise
                             (make-link-promise score value))))
             (xrp-vals xrp)
             (xrp-probs xrp)))

 (define (build-graph:terminal graph terminal last-id link-promise)
   (let ([terminal-id (terminal-id terminal)])
     (graph:realize-link-promise! graph last-id terminal-id link-promise)
     (graph:notify-ancestors-of-connection! graph terminal-id last-id)))

 (define (build-graph:recur graph recur last-id link-promise)
   (let* ([subthunk (lambda () (reset (make-terminal
                                  (apply (recur-call recur)
                                         (recur-state recur)))))]
          [subroot-id (recur-id recur)]
          [subroot-link-promise (make-link-promise 0.0 #t)]
          [subroot-is-new (graph:add/retrieve! graph subroot-id)]
          [callback (recursive-mem
                     (lambda (terminal-id)
                       (let ([value (terminal-id->value terminal-id)])
                         (build-graph graph
                                      (lambda () ((recur-cont recur) value))
                                      last-id
                                      (multiply-link-promises
                                       link-promise
                                       (make-link-promise (make-score-ref subroot-id
                                                                          terminal-id)
                                                          value)))))
                     (lambda () #f))])
     (graph:register-callback! graph subroot-id callback)
     (if subroot-is-new
         (build-graph graph subthunk subroot-id subroot-link-promise)
         (map callback (graph:reachable-terminals graph subroot-id)))))

 )