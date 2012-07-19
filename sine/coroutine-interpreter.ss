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
         subcall-cont
         subcall-proc
         subcall-args
         subcall?
         make-subcall
         apply-subcall
         make-terminal
         terminal-value
         terminal?)

 (import (rnrs)
         (scheme-tools value-number)
         (scheme-tools)
         (sine delimcc-simple-r6rs)
         (sine interpreter)
         (sine syntax))

 ;; Return at xrps and subcallsive calls

 (define-record-type xrp
   (fields cont vals probs))

 (define-record-type subcall
   (fields cont proc args))

 (define-record-type terminal
   (fields value))

 (define (coroutine-source vs logps)
   (shift f (make-xrp f vs logps)))

 (define (coroutine-subcall expr env)
   (if (or (syntax:application? expr)
           (syntax:if? expr))
       ;; (syntax:cache? expr)
       (let ([g (lambda (ex en) (interpreter-eval ex en coroutine-subcall coroutine-source))])
         (shift f (make-subcall f g (&cons expr env))))
       (interpreter-eval expr env coroutine-subcall coroutine-source)))

 (define (coroutine-interpreter expr)
   (reset (make-terminal (interpreter expr coroutine-subcall))))

 (define (apply-subcall subcall)
   (let ([syntax+env (&expand-pair (subcall-args subcall))])
     (apply (subcall-proc subcall)
            (list (car syntax+env)
                  (cdr syntax+env)))))

 ;; Return only at xrps

 (define coroutine-source/xrp coroutine-source)

 (define (coroutine-subcall/xrp expr env)
   (interpreter-eval expr env coroutine-subcall/xrp coroutine-source/xrp))

 (define (coroutine-interpreter/xrp expr)
   (reset (make-terminal (interpreter expr coroutine-subcall/xrp))))

 )