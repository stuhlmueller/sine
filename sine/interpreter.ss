#!r6rs

;; Based on metacircular interpreter in SICP and MIT-CHURCH.
;;
;; Extensions:
;; - added random primitives
;; - interpreter takes two additional arguments:
;;   - a function that will be called instead of the interpreter on recursive calls
;;   - a function that will be called when the interpreter needs randomness
;; - all data structures are "compressed" value-number objects
;; - environments are vectors of relevant variables

(library

 (sine interpreter)

 (export (rename (eval interpreter-eval))
         interpreter
         sicp-interpreter
         default-source
         make-default-subcall)

 (import (rename (rnrs)
                 (apply scheme-apply))
         (sine preamble)
         (sine env-flat)
         (sine syntax)
         (sine desugar)
         (sine primitives)
         (scheme-tools value-number)
         (scheme-tools srfi-compat :1)
         (only (scheme-tools) true? false? rest pair tagged-list? compose pe)
         (only (scheme-tools math) random-real))


 ;; --------------------------------------------------------------------
 ;; Eval

 (define (eval-dispatch-fn syntax)
   (cond [(syntax:self-evaluating? syntax) eval-self-evaluating]
         [(syntax:variable? syntax) eval-variable]
         [(syntax:quoted? syntax) eval-quoted]
         [(syntax:lambda? syntax) eval-lambda]
         [(syntax:if? syntax) eval-if]
         [(syntax:cache? syntax) eval-cache]
         [(syntax:application? syntax) eval-application]
         [else (error syntax "eval: unknown expression type")]))

 (define (eval syntax env subcall source)
   (let ([dispatch-fn (eval-dispatch-fn syntax)])
     (dispatch-fn syntax env subcall source)))

 (define (eval-if syntax env subcall source)
   (if (true? (&expand-boolean (subcall (if-syntax->if-predicate syntax) env)))
       (subcall (if-syntax->if-consequent syntax) env)
       (subcall (if-syntax->if-alternative syntax) env)))

 (define (eval-cache syntax env subcall source)
   (subcall (cache-syntax->content syntax) env))

 (define (eval-lambda syntax env subcall source)
   (make-procedure (lambda-syntax->lambda-parameters syntax)
                   (lambda-syntax->lambda-body syntax)
                   (restrict-environment (syntax->&free-vars syntax)
                                         env)))

 (define (eval-quoted syntax env subcall source)
   (quote-syntax->text-of-quotation syntax))

 (define (eval-variable syntax env subcall source)
   (let [(closure-address (variable-syntax->lexical-address syntax))]
     (lookup-value-by-id closure-address env)))

 (define (eval-self-evaluating syntax env subcall source)
   (self-evaluating-syntax->value syntax))

 (define (eval-application syntax env subcall source)
   (apply (subcall (application-syntax->operator-syntax syntax) env)
          (list-of-values (application-syntax->operands-syntax syntax) env subcall)
          subcall
          source))

 (define (list-of-values exps env subcall)
   (if (null? exps)
       '()
       (cons (subcall (first exps) env)
             (list-of-values (rest exps) env subcall))))

 (define (eval-sequence exps env subcall)
   (cond ((null? (cdr exps)) (subcall (first exps) env))
         (else (subcall (first exps) env)
               (eval-sequence (rest exps) env subcall))))


 ;; --------------------------------------------------------------------
 ;; Apply

 (define (apply? &proc)
   (and (&symbol?->b &proc)
        (eq? (&expand-symbol &proc) 'apply)))

 (define (apply-dispatch-fn procedure)
   (cond [(apply? procedure) apply-apply]
         [(deterministic-primitive-procedure? procedure) apply-deterministic-primitive-procedure]
         [(stochastic-primitive-procedure? procedure) apply-stochastic-primitive-procedure]
         [(compound-procedure? procedure) apply-compound-procedure]
         [else (begin
                 (pe "got: " procedure "\n"
                     "expanded: " (&expand-recursive procedure) "\n")
                 (error procedure "apply: unknown procedure type"))]))

 (define (apply procedure arguments subcall source)
   (let ([dispatch-fn (apply-dispatch-fn procedure)])
     (dispatch-fn procedure arguments subcall source)))

 (define (bindings params vals)
   (cond [(null? params) (pair '() '())]
         [(symbol? params) (pair (list params)
                                 (if (symbol? vals)
                                     (list vals)
                                     (list (compress-list vals))))]
         [else (if (null? vals)
                   (error (pair params vals)
                          "bindings: not enough arguments!")
                   (let ([remainder (bindings (rest params) (rest vals))])
                     (pair (pair (first params) (first remainder))
                           (pair (first vals) (rest remainder)))))]))

 (define (apply-compound-procedure proc args subcall source)
   (let* ([binds (bindings (procedure-parameters proc) args)]
          [binding-vars (first binds)]
          [binding-vals (rest binds)])
     (subcall (procedure-body proc)
              (extend-environment binding-vars
                                  binding-vals
                                  (procedure-environment proc)))))

 (define (apply-apply proc args subcall source)
   (apply (car args)
          (&expand-list (cadr args))
          subcall source))


 ;; --------------------------------------------------------------------
 ;; Procedure data type (compressed)

 (define (make-procedure parameters &body &env)
   (&list (compress-symbol 'procedure)
          (compress-recursive parameters)
          &body
          &env))

 (define (compound-procedure? &p)
   (&tagged-list?->b &p 'procedure))

 (define (procedure-parameters p)
   (&expand-recursive (&cadr p)))

 (define (procedure-body p)
   (&caddr p))

 (define (procedure-environment p)
   (&cadddr p))


 ;; --------------------------------------------------------------------
 ;; Primitive procedures, generic (compressed)

 (define (primitive-implementation &proc)
   (&expand-procedure (&cadr &proc)))

 (define (primitive-procedure-names procs)
   (map car procs))

 (define (primitive-procedure-objects procs type-symbol)
   (map (lambda (proc) (&list (compress-symbol type-symbol)
                         (compress-procedure (cadr proc) (car proc))))
        procs))

 (define (all-primitive-names)
   (append (primitive-procedure-names deterministic-primitive-procedures)
           (primitive-procedure-names stochastic-primitive-procedures)
           (map car primitive-constants)))

 (define (all-primitive-objects)
   (append (primitive-procedure-objects deterministic-primitive-procedures 'primitive)
           (primitive-procedure-objects stochastic-primitive-procedures 'rand)
           (map second primitive-constants)))


 ;; --------------------------------------------------------------------
 ;; Deterministic primitive procedures (compressed)

 (define (deterministic-primitive-procedure? &proc)
   (&tagged-list?->b &proc 'primitive))

 (define (apply-deterministic-primitive-procedure &proc args subcall source)
   (scheme-apply (primitive-implementation &proc)
                 args))


 ;; --------------------------------------------------------------------
 ;; Stochastic primitive procedures (compressed)

 (define (stochastic-primitive-procedure? &proc)
   (&tagged-list?->b &proc 'rand))

 (define (apply-stochastic-primitive-procedure &proc args subcall source)
   (scheme-apply (primitive-implementation &proc)
                 (cons source args)))


 ;; --------------------------------------------------------------------
 ;; Top-level

 (define (setup-environment)
   (extend-environment (all-primitive-names)
                       (all-primitive-objects)
                       (the-empty-environment)))

 (define default-source
   (lambda (p) (< (random-real) p)))

 (define (make-default-subcall source)
   (letrec ([subcall (lambda (syntax env)
                       (begin
                         (assert (syntax? syntax))
                         (eval syntax env subcall source)))])
     subcall))

 (define (interpreter expr subcall)
   (let ([env (setup-environment)])
     (subcall (sexpr->syntax expr env) env)))

 (define (sicp-interpreter expr)
   (let ([env (setup-environment)]
         [subcall (make-default-subcall default-source)])
     (&expand-recursive (subcall (sexpr->syntax (with-preamble expr) env) env))))

 )