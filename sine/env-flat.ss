#!r6rs

;; Flat relevant environments

(library

 (sine env-flat)

 (export the-empty-environment
         extend-environment
         lookup-variable-value-and-id
         lookup-value-by-id
         lookup-variable-id
         restrict-environment)

 (import (rnrs)
         (sine value-number)
         (scheme-tools srfi-compat :43)
         (scheme-tools srfi-compat :1))


 ;; --------------------------------------------------------------------
 ;; Environment ADT (compressed)
 ;;
 ;; Env is a vector of (name . val) pairs.
 ;; In extend-environment, vals is a list of compressed &val's

 (define (the-empty-environment) (&vector))

 (define (extend-environment vars vals &base-env)
   (&vector-append (apply &vector
                          (map (lambda (v n) (&cons (compress-symbol v) n))
                               vars
                               vals))
                   &base-env))

 ;; --------------------------------------------------------------------
 ;; Variable lookup
 ;;
 ;; Address is a vector index

 (define (lookup-variable-value-and-id var &top-env)
   (let ([address (&vector-index (lambda (v) (eq? var (car v))) &top-env)])
     (cons (&cdr (&vector-ref &top-env address))
           address)))

 (define (lookup-value-by-id address &env)
   (&cdr (&vector-ref &env address)))

 (define (lookup-variable-id var &env)
   (let ((value-and-id (lookup-variable-value-and-id var &env)))
     (cdr value-and-id)))


 ;; --------------------------------------------------------------------
 ;; Make new environment based on a subset of addresses

 (define (restrict-environment &relevant-ids &env)
   (compress-vector
    (vector-map (lambda (i) (&vector-ref &env i))
                (&expand-recursive &relevant-ids))))

 )