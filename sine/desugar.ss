#!r6rs

;; based on mit-church desugar

(library

 (sine desugar)

 (export desugar
         desugar-all
         register-sugar!)

 (import (rnrs)
         (scheme-tools)
         (scheme-tools srfi-compat :1))

 (define sugar-registry '())

 (define (register-sugar! pattern translator)
   (set! sugar-registry (cons (cons pattern translator) sugar-registry)) )

 (define sugar-pattern car)

 (define sugar-translator cdr)

 (define (desugar expr)
   (define unchanged (gensym))
   (define (try expr sugar-list)
     (if (null? sugar-list)
         unchanged
         (if ((sugar-pattern (first sugar-list)) expr)
             ((sugar-translator (first sugar-list)) expr)
             (try expr (rest sugar-list)) )))
   (let loop ((expr expr))
     (let ((new-expr (try expr sugar-registry)))
       (if (eq? new-expr unchanged)
           expr
           (loop new-expr) ))))

 (define (desugar-all sexpr)
   (let ((new-sexpr (desugar sexpr)))
     (if (list? new-sexpr)
         (map desugar-all new-sexpr)
         new-sexpr)))

 (define (begin-wrap exprs)
   (if (null? (rest exprs))
       (first exprs)
       `(begin ,@exprs)))

 (define (let? expr)
   (and (tagged-list? expr 'let) (list? (second expr))))

 (define (let->lambda expr)
   (let* ((bindings (second expr))
          (vars (map first bindings))
          (value-exprs (map second bindings))
          (body (begin-wrap (drop expr 2))))
     `((lambda ,vars ,body) ,@value-exprs) ))

 (define (named-let? expr)
   (and (tagged-list? expr 'let) (symbol? (second expr))))

 (define (named-let->lambda expr)
   (let* ((proc-name (second expr))
          (let-conversion (let->lambda (rest expr))))
     `((Y (lambda (,proc-name) ,(first let-conversion))) ,@(rest let-conversion)) ))

 (define (let*? expr) (tagged-list? expr 'let*))

 (define (desugar-let* expr)
   (let ((bindings (second expr))
         (body (begin-wrap (drop expr 2))))
     (if (null? bindings)
         body
         (let* ((binding (first bindings))
                (var (first binding))
                (value-exprs (second binding)) )
           `((lambda (,var) (let* ,(rest bindings) ,body)) ,value-exprs) ))))

 (define (case? expr) (tagged-list? expr 'case))

 (define (desugar-case expr)
   (let ((key-symbol (gensym))
         (key-expr (second expr))
         (value-exprs (drop expr 2)) )
     `(let ((,key-symbol ,key-expr))
        (cond ,@(map (lambda (value-expr)
                       (let ((values (first value-expr))
                             (val-expr (rest value-expr)) )
                         (cond ((list? values)
                                `((any (list ,@(map (lambda (val) `(equal? ,key-symbol ,val)) values) ))
                                  ,@val-expr ) )
                               ((equal? values 'else)
                                `(else ,@val-expr) )
                               (else (error "Invalid case expression." values)) ) ))
                     value-exprs ))) ))

 (define (cond? expr) (tagged-list? expr 'cond))

 (define (desugar-cond expr)
   (let loop ((conditions (rest expr)))
     (if (null? conditions)
         '(void)
         (let* ((condition (first conditions))
                (test (first condition)))
           (if (equal? test 'else)
               (if (not (null? (rest conditions)))
                   (error "else clause in cond expression must be last.")
                   (begin-wrap (rest condition)) )
               `(if ,test
                    ,(begin-wrap (rest condition))
                    ,(loop (rest conditions)) ) )))))

 (define (mem-rec? expr) (tagged-list? expr 'mem-rec))

 (define (desugar-mem-rec expr)
   (if (not (eq? (first (third expr)) 'lambda))
       (error "second argument to mem-rec must be lambda expression.")
       (let ((fn-symbol (second expr))
             (fn-args (second (third expr)))
             (fn-body (third (third expr)))
             (F (gensym))
             (G (gensym)))
         `(let ((,F
                 (mem
                  (lambda ,(pair G fn-args)
                    (let ((,fn-symbol (lambda ,fn-args (,G ,G ,@fn-args))))
                      ,fn-body)))))
            (lambda ,fn-args (,F ,F ,@fn-args))))))

 (define (define-fn? expr)
   (and (tagged-list? expr 'define) (not (symbol? (second expr)))))

 (define (desugar-define-fn expr)
   (let ((def-var (first (second expr)))
         (def-params (rest (second expr)))
         (def-body (rest (rest expr))))
     `(define ,def-var (lambda ,def-params ,@def-body))))

 (define (rejection? expr)
   (tagged-list? expr 'rejection-query))

 (define (desugar-rejection expr)
   `(nfqp-rejection-query
     (lambda () (begin ,@(drop-right (rest expr) 2)
                  (pair ,(list-ref expr (- (length expr) 2)) ,(last expr))))))

 (register-sugar! let? let->lambda)
 (register-sugar! let*? desugar-let*)
 (register-sugar! named-let? named-let->lambda)
 (register-sugar! case? desugar-case)
 (register-sugar! cond? desugar-cond)
 (register-sugar! mem-rec? desugar-mem-rec)
 (register-sugar! define-fn? desugar-define-fn)
 (register-sugar! rejection? desugar-rejection)

 )