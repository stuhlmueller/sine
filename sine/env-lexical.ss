#!r6rs

;; Environments with lexical addressing based on mit-church/sicp

(library

 (sine env-lexical)

 (export the-empty-environment
         extend-environment
         lookup-variable-value-and-id
         lookup-value-by-id
         lookup-variable-id)

 (import (rnrs)
         (scheme-tools srfi-compat :43)
         (scheme-tools srfi-compat :1))


 ;; --------------------------------------------------------------------
 ;; Frame ADT

 (define (make-frame variables values)
   (vector (list->vector variables) (list->vector values)))

 (define (first-frame env) (car env))

 (define (frame-variables frame) (vector-ref frame 0))

 (define (frame-values frame) (vector-ref frame 1))


 ;; --------------------------------------------------------------------
 ;; Environment ADT

 (define the-empty-environment (list))

 (define (enclosing-environment env)
   (cdr env))

 (define (extend-environment vars vals base-env)
   (cons (make-frame vars vals) base-env))


 ;; --------------------------------------------------------------------
 ;; Variable lookup
 ;;
 ;; Address is (frame-loc . var-loc)

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
     (env-loop top-env)))

 (define (lookup-value-by-id address env)
   (vector-ref (frame-values (list-ref env (car address)))
               (cdr address)))

 (define (lookup-variable-id var env)
   (let ((value-and-id (lookup-variable-value-and-id var env)))
     (cdr value-and-id)))

 )