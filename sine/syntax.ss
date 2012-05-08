#!r6rs

;; based on mit-church syntax.ss

(library

 (sine syntax)

 (export syntax:self-evaluating?
         syntax:variable?
         syntax:quoted?
         syntax:lambda?
         lambda-syntax->lambda-parameters
         lambda-syntax->lambda-body
         syntax:if?
         if-syntax->if-predicate
         if-syntax->if-consequent
         if-syntax->if-alternative
         syntax:application?
         quote-syntax->text-of-quotation
         syntax:definition?
         definition-syntax->definition-variable
         definition-syntax->definition-value
         variable-syntax->lexical-address
         self-evaluating-syntax->value
         application-syntax->operator-syntax
         application-syntax->operands-syntax
         sexpr->syntax)

 (import (rnrs)
         (scheme-tools srfi-compat :1)
         (only (scheme-tools) tagged-list? rest pair)
         (sine desugar)
         (sine env-lexical))


 ;; Syntax ADT

 (define (make-syntax type original-expr expr)
   (vector 'syntax type original-expr expr))
 (define (syntax? s) (and (vector? s) (eq? 'syntax (vector-ref s 0))))
 (define (syntax->type s) (vector-ref s 1))
 (define (syntax->expr s) (vector-ref s 3))

 (define (syntax:is-type sym) (lambda (sobj) (eq? sym (syntax->type sobj))))
 (define syntax:self-evaluating? (syntax:is-type 'self-evaluating))
 (define syntax:quoted? (syntax:is-type 'quoted))
 (define syntax:variable? (syntax:is-type 'variable))
 (define syntax:begin? (syntax:is-type 'begin))
 (define syntax:definition? (syntax:is-type 'definition))
 (define syntax:lambda? (syntax:is-type 'lambda))
 (define syntax:if? (syntax:is-type 'if))
 (define syntax:get-env? (syntax:is-type 'get-env))
 (define syntax:application? (syntax:is-type 'application))

 (define (quote-syntax->text-of-quotation syntax)
   (text-of-quotation (syntax->expr syntax)))

 (define (definition-syntax->definition-variable syntax)
   (second (syntax->expr syntax)))

 (define (definition-syntax->definition-value syntax)
   (third (syntax->expr syntax)))

 (define (lambda-syntax->lambda-parameters syntax)
   (lambda-parameters (syntax->expr syntax)))

 (define (lambda-syntax->lambda-body syntax)
   (lambda-body (syntax->expr syntax)))

 (define (if-syntax->if-predicate syntax)
   (if-predicate (syntax->expr syntax)))

 (define (if-syntax->if-consequent syntax)
   (if-consequent (syntax->expr syntax)))

 (define (if-syntax->if-alternative syntax)
   (if-alternative (syntax->expr syntax)))

 (define (variable-syntax->lexical-address syntax)
   (syntax->expr syntax))

 (define (self-evaluating-syntax->value syntax)
   (syntax->expr syntax))

 (define (application-syntax->operator-syntax syntax)
   (first (syntax->expr syntax)))

 (define (application-syntax->operands-syntax syntax)
   (rest (syntax->expr syntax)))


 ;; SEXPRs

 (define (self-evaluating? exp)
   (cond ((boolean? exp) #t)
         ((number? exp) #t)
         ((string? exp) #t)
         ((char? exp) #t)
         ((equal? exp '()) #t)
         ;; ((eq? exp 'eval) #t)
         ;; ((eq? exp 'apply) #t)
         ;; ((eq? exp 'mem) #t)
         ;; ((eq? exp 'make-xrp) #t)
         (else #f)))

 (define (quoted? exp)
   (tagged-list? exp 'quote))

 (define (text-of-quotation exp)
   (cadr exp))

 (define (variable? exp) (symbol? exp))

 (define (begin? expr)  (tagged-list? expr 'begin))

 (define (definition? exp)
   (tagged-list? exp 'define))

 (define (definition-variable exp)
   (if (symbol? (cadr exp))
       (cadr exp)
       (caadr exp)))

 (define (definition-value exp)
   (if (symbol? (cadr exp))
       (caddr exp)
       (make-lambda (cdadr exp)
                    (cddr exp))))

 (define (lambda? exp) (tagged-list? exp 'lambda))

 (define (lambda-parameters exp) (cadr exp))

 (define (lambda-body exp) (caddr exp))

 (define (make-lambda parameters body)
   (cons 'lambda (cons parameters body)))

 (define (if? exp) (tagged-list? exp 'if))

 (define (if-predicate exp) (cadr exp))

 (define (if-consequent exp) (caddr exp))

 (define (get-env? exp)
   (tagged-list? exp 'get-current-environment))

 (define (if-alternative exp)
   (if (not (null? (cdddr exp)))
       (cadddr exp)
       'false))

 (define (application? exp) (pair? exp))

 (define (top-level? expr) (tagged-list? expr 'top-level))

 (define top-level-sequence rest)


 ;; SEXPR to Syntax

 (define (plist->list params)
   (cond ((symbol? params) (list params))
         ((null? params) (list))
         (else (pair (first params)
                     (plist->list (rest params)) ))))

 (define (sexpr->syntax sugared-sexpr env)

   (with-exception-handler

    (lambda (exn)
      (begin (display "\nSyntax error: ")
             (write sugared-sexpr)
             (newline)
             (if (string? exn)
                 (error exn "Syntax error")
                 (raise-continuable exn) )))

    (lambda ()
      (let ((sexpr (desugar sugared-sexpr))
            (recurse (lambda (subexpr) (sexpr->syntax subexpr env)) ))
        (cond
         ((self-evaluating? sexpr) (make-syntax 'self-evaluating sugared-sexpr sexpr) )
         ((variable? sexpr) (let ((lexical-address (rest (lookup-variable-value-and-id sexpr env))))
                              (make-syntax 'variable sugared-sexpr lexical-address) ))
         ((quoted? sexpr) (make-syntax 'quoted sugared-sexpr sexpr))
         ((lambda? sexpr) (let* ((formal-parameters (lambda-parameters sexpr))
                                 (body (sexpr->syntax (lambda-body sexpr)
                                                      (extend-environment (plist->list formal-parameters)
                                                                          (plist->list formal-parameters)
                                                                          env ))))
                            (make-syntax 'lambda sugared-sexpr `(lambda ,formal-parameters ,body))))
         ((get-env? sexpr) (make-syntax 'get-env sugared-sexpr sexpr))
         ((if? sexpr) (make-syntax 'if sugared-sexpr  (cons 'if (map recurse (rest sexpr)))))
         ((application? sexpr) (make-syntax 'application sugared-sexpr (map recurse sexpr)))
         (else (error "Unknown expression type:" sexpr)) )))))

 (define (extract-defined-vars seq)
   (filter-map (lambda (expr) (if (definition? expr) (definition-variable expr) #f)) seq))

 )