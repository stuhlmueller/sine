#!r6rs

;; based on mit-church syntax.ss

(library

 (sine syntax)

 (export syntax:self-evaluating?
         syntax:variable?
         syntax:quoted?
         syntax:lambda?
         syntax?
         lambda-syntax->lambda-parameters
         lambda-syntax->lambda-body
         syntax:if?
         if-syntax->if-predicate
         if-syntax->if-consequent
         if-syntax->if-alternative
         syntax:application?
         quote-syntax->text-of-quotation
         variable-syntax->lexical-address
         self-evaluating-syntax->value
         application-syntax->operator-syntax
         application-syntax->operands-syntax
         sexpr->syntax)

 (import (rnrs)
         (scheme-tools srfi-compat :1)
         (only (scheme-tools) tagged-list? rest pair pretty-print)
         (sine desugar)
         (sine value-number)
         (sine env-lexical))


 ;; --------------------------------------------------------------------
 ;; Syntax ADT (compressed)

 (define (make-syntax type original-expr &sub)
   (compress-vector (vector (compress-symbol 'syntax)
                            (compress-symbol type)
                            (compress-recursive original-expr)
                            &sub)))

 (define (syntax? s)
   (and (&vector? s)
        (eq? 'syntax (&expand-symbol (&vector-ref s 0)))))

 (define (syntax->type s)
   (&expand-symbol (&vector-ref s 1)))

 (define (syntax->&sub s)
   (&vector-ref s 3))

 (define (syntax:is-type sym) (lambda (sobj) (eq? sym (syntax->type sobj))))
 (define syntax:self-evaluating? (syntax:is-type 'self-evaluating))
 (define syntax:quoted? (syntax:is-type 'quoted))
 (define syntax:variable? (syntax:is-type 'variable))
 (define syntax:begin? (syntax:is-type 'begin))
 (define syntax:lambda? (syntax:is-type 'lambda))
 (define syntax:if? (syntax:is-type 'if))
 (define syntax:application? (syntax:is-type 'application))

 (define (quote-syntax->text-of-quotation syntax)
   (&expand-recursive (syntax->&sub syntax)))

 (define (lambda-syntax->lambda-parameters syntax)
   (&expand-recursive (&car (syntax->&sub syntax))))

 (define (lambda-syntax->lambda-body syntax)
   (&cadr (syntax->&sub syntax)))

 (define (if-syntax->if-predicate syntax)
   (&car (syntax->&sub syntax)))

 (define (if-syntax->if-consequent syntax)
   (&cadr (syntax->&sub syntax)))

 (define (if-syntax->if-alternative syntax)
   (&caddr (syntax->&sub syntax)))

 (define (variable-syntax->lexical-address syntax)
   (&expand-recursive (syntax->&sub syntax)))

 (define (self-evaluating-syntax->value syntax)
   (&expand-recursive (syntax->&sub syntax)))

 (define (application-syntax->operator-syntax syntax)
   (&car (syntax->&sub syntax)))

 (define (application-syntax->operands-syntax syntax)
   (&expand-list (&cdr (syntax->&sub syntax))))


 ;; --------------------------------------------------------------------
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

 (define (variable? exp) (symbol? exp))

 (define (begin? expr)  (tagged-list? expr 'begin))

 (define (lambda? exp) (tagged-list? exp 'lambda))

 (define (make-lambda parameters body)
   (cons 'lambda (cons parameters body)))

 (define (lambda-parameters exp) (cadr exp))

 (define (lambda-body exp) (caddr exp))

 (define (if? exp) (tagged-list? exp 'if))

 (define (get-env? exp)
   (tagged-list? exp 'get-current-environment))

 (define (application? exp) (pair? exp))

 (define (top-level? expr) (tagged-list? expr 'top-level))

 (define top-level-sequence rest)


 ;; --------------------------------------------------------------------
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

         [(self-evaluating? sexpr)
          (make-syntax 'self-evaluating
                       sugared-sexpr
                       (compress-recursive sexpr))]

         [(variable? sexpr)
          (let ((lexical-address (lookup-variable-id sexpr env)))
            (make-syntax 'variable
                         sugared-sexpr
                         (compress-recursive lexical-address)))]

         [(quoted? sexpr)
          (make-syntax 'quoted
                       sugared-sexpr
                       (compress-recursive (cadr sexpr)))]

         [(lambda? sexpr)
          (let* ((formal-parameters (lambda-parameters sexpr))
                 (body (sexpr->syntax (lambda-body sexpr)
                                      (extend-environment (plist->list formal-parameters)
                                                          (plist->list formal-parameters)
                                                          env))))
            (make-syntax 'lambda
                         sugared-sexpr
                         (compress-list (list (compress-recursive formal-parameters) body))))]

         [(if? sexpr)
          (make-syntax 'if
                       sugared-sexpr
                       (compress-list (map recurse (rest sexpr))))]

         [(application? sexpr)
          (make-syntax 'application
                       sugared-sexpr
                       (compress-list (map recurse sexpr)))]

         [else (error "Unknown expression type:" sexpr)] )))))

 )