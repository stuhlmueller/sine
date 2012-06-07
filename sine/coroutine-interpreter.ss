#!r6rs

(library

 (sine coroutine-interpreter)

 (export coroutine-interpreter
         coroutine-interpreter/xrp
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
         (scheme-tools)
         (sine syntax)
         (sine interpreter)
         (sine value-number)
         (sine delimcc-simple-r6rs))

 ;; Return at xrps and recursive calls

 (define-record-type xrp
   (fields cont vals probs))

 (define-record-type recur
   (fields cont call state))

 (define-record-type terminal
   (fields value))

 (define (coroutine-source vs logps)
   (shift f (make-xrp f vs logps)))

 (define (coroutine-recur expr env)
   (if (syntax:application? expr) ;; (syntax:cache? expr)
       (let ([g (lambda (ex en) (interpreter-eval ex en coroutine-recur coroutine-source))])
         (shift f (make-recur f g (&cons expr env))))
       (interpreter-eval expr env coroutine-recur coroutine-source)
       ))

 (define (coroutine-interpreter expr)
   (reset (make-terminal (interpreter expr coroutine-recur))))

 ;; Return only at xrps

 (define coroutine-source/xrp coroutine-source)

 (define (coroutine-recur/xrp expr env)
   (interpreter-eval expr env coroutine-recur/xrp coroutine-source/xrp))

 (define (coroutine-interpreter/xrp expr)
   (reset (make-terminal (interpreter expr coroutine-recur/xrp))))

 )