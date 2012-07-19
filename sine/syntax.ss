#!r6rs

;; based on mit-church syntax.ss

(library

 (sine syntax)

 (export syntax:cache?
         cache-syntax->content
         syntax:self-evaluating?
         syntax:variable?
         syntax:quoted?
         syntax:lambda?
         syntax?
         syntax->&free-vars
         syntax->original-expr
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

 (import (only (scheme-tools) tagged-list? rest pair pretty-print pe)
         (rnrs)
         (scheme-tools srfi-compat :1)
         (scheme-tools value-number)
         (sine desugar)
         (sine desugar-Y)
         (sine env-flat)
         (sine sexpr))


 ;; --------------------------------------------------------------------
 ;; Syntax ADT (compressed)

 (define (make-syntax type original-expr &sub . free-vars)
   (compress-vector (vector (compress-symbol 'syntax)
                            (compress-symbol type)
                            &sub
                            (if (null? free-vars)
                                (compress-boolean #f)
                                (compress-recursive (car free-vars)))
                            (compress-recursive original-expr) ;; remove eventually
                            )))

 (define (syntax? s)
   (and (&vector?->b s)
        (eq? 'syntax (&expand-symbol (&vector-ref s 0)))))

 (define (syntax->type s)
   (&expand-symbol (&vector-ref s 1)))

 (define (syntax->&sub s)
   (&vector-ref s 2))

 (define (syntax->&free-vars s)
   (&vector-ref s 3))

 (define (syntax->original-expr s)
   (&expand-recursive (&vector-ref s 4)))

 (define (syntax:is-type sym) (lambda (sobj) (eq? sym (syntax->type sobj))))
 (define syntax:self-evaluating? (syntax:is-type 'self-evaluating))
 (define syntax:quoted? (syntax:is-type 'quoted))
 (define syntax:variable? (syntax:is-type 'variable))
 (define syntax:begin? (syntax:is-type 'begin))
 (define syntax:lambda? (syntax:is-type 'lambda))
 (define syntax:if? (syntax:is-type 'if))
 (define syntax:application? (syntax:is-type 'application))
 (define syntax:cache? (syntax:is-type 'cache))

 (define (cache-syntax->content syntax)
   (syntax->&sub syntax))

 (define (quote-syntax->text-of-quotation syntax)
   (syntax->&sub syntax))

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
   (syntax->&sub syntax))

 (define (application-syntax->operator-syntax syntax)
   (&car (syntax->&sub syntax)))

 (define (application-syntax->operands-syntax syntax)
   (&expand-list (&cdr (syntax->&sub syntax))))


 ;; --------------------------------------------------------------------
 ;; SEXPR to Syntax

 (define (sexpr->syntax sugared-sexpr env)

   (let loop ([sugared-sexpr (desugar-all-Y sugared-sexpr)]
              [env env])

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
              (recurse (lambda (subexpr) (loop subexpr env)) ))
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

           [(cache? sexpr)
            (make-syntax 'cache
                         sugared-sexpr
                         (recurse (cadr sexpr)))]

           [(lambda? sexpr)
            (let* ([formal-parameters (lambda-parameters sexpr)]
                   [relevant-ids
                    (list->vector (map (lambda (var) (lookup-variable-id var env))
                                       (free-variables (desugar-all sexpr)
                                                       (append self-eval-vars
                                                               (plist->list formal-parameters)))))]
                   [body (loop (lambda-body sexpr)
                               (extend-environment (plist->list formal-parameters)
                                                   (map compress-symbol
                                                        (plist->list formal-parameters))
                                                   (restrict-environment relevant-ids env)))])
              (make-syntax 'lambda
                           sugared-sexpr
                           (compress-list (list (compress-recursive formal-parameters) body))
                           relevant-ids))]

           [(if? sexpr)
            (make-syntax 'if
                         sugared-sexpr
                         (compress-list (map recurse (rest sexpr))))]

           [(application? sexpr)
            (make-syntax 'application
                         sugared-sexpr
                         (compress-list (map recurse sexpr)))]

           [else (error "Unknown expression type:" sexpr)] ))))))

 )