#!r6rs

(library

 (sine coroutine-interpreter)

 (export coroutine-interpreter
         make-xrp
         xrp-cont
         xrp-vals
         xrp-probs
         xrp?
         make-recur
         recur-cont
         recur-call
         recur-state
         recur?
         make-terminal
         terminal-value
         terminal?)

 (import (rnrs)
         (sine interpreter)
         (sine delimcc-simple-r6rs))

 (define-record-type xrp
   (fields cont vals probs))

 (define-record-type recur
   (fields cont call state))

 (define-record-type terminal
   (fields value))

 (define (coroutine-source p)
   (shift f (make-xrp f '(#t #f) (list (log p) (log (- 1 p))))))

 (define (coroutine-recur expr env)
   (let ([g (lambda (ex en) (interpreter-eval ex en coroutine-recur coroutine-source))])
     (shift f (make-recur f g (list expr env)))))

 (define (coroutine-interpreter expr)
   (reset (make-terminal (interpreter expr
                                      coroutine-recur
                                      coroutine-source))))

 )