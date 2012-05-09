#!r6rs

;; based on mit-church/sicp env-lexical
;;
;; this is a version of environment handling code that uses lexical
;; addressing.

(library

 (sine env-lexical)

 (export enclosing-environment
         address-in-enclosing-environment
         first-frame
         the-empty-environment
         make-frame
         frame-variables
         frame-values
         first-frame-values
         extend-environment
         lookup-variable-value-and-id
         lookup-value-by-id
         lookup-variable-value)

 (import (rnrs)
         (scheme-tools srfi-compat :43)
         (scheme-tools srfi-compat :1))

 (define (enclosing-environment env)
   (cdr env))

 ;; adjust a lexical address for passing through a lambda: pop top frame.
 ;; if frame was already 0 (first frame), return null.
 (define (address-in-enclosing-environment lexical-address)
   (if (not (pair? lexical-address)) ; if its not a lexical address, just return it...
       lexical-address
       (if (= (car lexical-address) 0)
           #f
           (cons (- (car lexical-address) 1) (cdr lexical-address)))))

 (define (first-frame env) (car env))

 (define the-empty-environment (list)) ;; we leave this a list so that mutation fails, as it should

 (define (make-frame variables values)
   (vector (list->vector variables) (list->vector values)))

 (define (frame-variables frame) (vector-ref frame 0))

 (define (frame-values frame) (vector-ref frame 1))

 (define (first-frame-values env)
   (vector->list (frame-values (first-frame env))))

 (define (extend-environment vars vals base-env)
   (cons (make-frame vars vals) base-env))

 ;; this returns both the value and the address as a cons (val
 ;; . addr), where address is (frame-loc . var-loc)
 (define (lookup-variable-value-and-id var top-env)
   (let ((frame-loc 0))
     (define (env-loop env)
       (define (find-var vars vals)
         (let ((var-index (vector-index (lambda (v) (eq? v var)) vars)))
           (if (not var-index)
               (begin (set! frame-loc (+ frame-loc 1))
                      (env-loop (enclosing-environment env)))
               (cons (vector-ref vals var-index) (cons frame-loc var-index)))))
       (if (eq? env the-empty-environment)
           (raise-continuable "Unbound variable")
           (let ((frame (first-frame env)))
             (find-var (frame-variables frame)
                       (frame-values frame) ))))
     (env-loop top-env) ))

 ;; this uses a lexical address to get a value in an environment,
 ;; returns both the value and the address
 (define (lookup-value-by-id address env)
   (cons (vector-ref (frame-values (list-ref env (car address)))
                     (cdr address))
         address))

 (define (lookup-variable-value var env)
   (let ((value-and-id (lookup-variable-value-and-id var env)))
     (car value-and-id)))

 )