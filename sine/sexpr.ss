#!r6rs

(library

 (sine sexpr)

 (export cache?
         cache-content
         self-evaluating?
         self-eval-vars
         quoted?
         variable?
         begin?
         lambda?
         make-lambda
         lambda-parameters
         lambda-body
         if?
         get-env?
         application?
         top-level?
         top-level-sequence
         plist->list
         free-variables
         replace-free-variable)

 (import (rnrs)
         (scheme-tools)
         (scheme-tools srfi-compat :1))

 (define self-eval-vars
   '(eval apply))

 (define (self-evaluating? exp)
   (cond ((boolean? exp) #t)
         ((number? exp) #t)
         ((string? exp) #t)
         ((char? exp) #t)
         ((equal? exp '()) #t)
         ((eq? exp 'apply) #t)
         ;; ((eq? exp 'eval) #t)
         ;; ((eq? exp 'mem) #t)
         ;; ((eq? exp 'make-xrp) #t)
         (else #f)))

 (define (quoted? exp)
   (tagged-list? exp 'quote))

 (define (cache? exp) (tagged-list? exp 'cache))

 (define cache-content cadr)

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

 (define (plist->list params)
   (cond ((symbol? params) (list params))
         ((null? params) '())
         (else (pair (first params)
                     (plist->list (rest params))))))

 (define (free-variables sexpr bound-vars)
   (cond
    ((begin? sexpr)
     (apply append
            (map (lambda (e) (free-variables e bound-vars))
                 (rest sexpr))))
    ((quoted? sexpr) '())
    ((lambda? sexpr)
     (free-variables (lambda-body sexpr)
                     (append (plist->list (lambda-parameters sexpr))
                             bound-vars)))
    ((if? sexpr)
     (apply append
            (map (lambda (e) (free-variables e bound-vars))
                 (rest sexpr))))
    ((application? sexpr)
     (apply append
            (map (lambda (e) (free-variables e bound-vars))
                 sexpr)))
    ((symbol? sexpr)
     (if (memq sexpr bound-vars)
         '()
         (list sexpr)))
    ((self-evaluating? sexpr) '())
    (else (error sexpr "free-variables: can't handle sexpr type"))))

 (define (replace-free-variable sexpr from to)
   (cond [(lambda? sexpr)
          (if (contains? (plist->list (lambda-parameters sexpr))
                         from
                         eq?)
              sexpr
              `(lambda ,(lambda-parameters sexpr)
                 ,(replace-free-variable (lambda-body sexpr)
                                         from to)))]
         [(list? sexpr)
          (map (lambda (s2) (replace-free-variable s2 from to))
               sexpr)]
         [(symbol? sexpr) (if (eq? sexpr from) to sexpr)]
         [(self-evaluating? sexpr) sexpr]
         [else (error sexpr "replace-free-variable: unknown sexpr type")]))

 )