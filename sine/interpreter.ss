#!r6rs

;; Based on metacircular interpreter in SICP and MIT-CHURCH.
;;
;; Extensions:
;; - added random primitives
;; - interpreter takes two additional arguments:
;;   - a function that will be called instead of the interpreter on recursive calls
;;   - a function that will be called when the interpreter needs randomness

(library

 (sine interpreter)

 (export (rename (eval interpreter-eval))
         interpreter
         sicp-interpreter
         default-source
         make-default-recur)

 (import (rename (rnrs)
                 (apply scheme-apply))
         (sine env-lexical)
         (sine syntax)
         (sine desugar)
         (sine primitives)
         (sine value-number)
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
         [(syntax:application? syntax) eval-application]
         [else (error syntax "eval: unknown expression type")]))

 (define (eval syntax env recur source)
   (let ([dispatch-fn (eval-dispatch-fn syntax)])
     (dispatch-fn syntax env recur source)))

 (define (eval-if syntax env recur source)
   (if (true? (&expand-boolean (recur (if-syntax->if-predicate syntax) env)))
       (recur (if-syntax->if-consequent syntax) env)
       (recur (if-syntax->if-alternative syntax) env)))

 (define (eval-lambda syntax env recur source)
   (make-procedure (lambda-syntax->lambda-parameters syntax)
                   (lambda-syntax->lambda-body syntax)
                   env))

 (define (eval-quoted syntax env recur source)
   (quote-syntax->text-of-quotation syntax))

 (define (eval-variable syntax env recur source)
   (let [(lexical-address (variable-syntax->lexical-address syntax))]
     (lookup-value-by-id lexical-address env)))

 (define (eval-self-evaluating syntax env recur source)
   (self-evaluating-syntax->value syntax))

 (define (eval-application syntax env recur source)
   (apply (recur (application-syntax->operator-syntax syntax) env)
          (list-of-values (application-syntax->operands-syntax syntax) env recur)
          recur
          source))

 (define (list-of-values exps env recur)
   (if (null? exps)
       '()
       (cons (recur (first exps) env)
             (list-of-values (rest exps) env recur))))

 (define (eval-sequence exps env recur)
   (cond ((null? (cdr exps)) (recur (first exps) env))
         (else (recur (first exps) env)
               (eval-sequence (rest exps) env recur))))


 ;; --------------------------------------------------------------------
 ;; Apply

 (define (apply-dispatch-fn procedure)
   (cond [(deterministic-primitive-procedure? procedure) apply-deterministic-primitive-procedure]
         [(stochastic-primitive-procedure? procedure) apply-stochastic-primitive-procedure]
         [(compound-procedure? procedure) apply-compound-procedure]
         [else (begin
                 (pe "got: " (&expand-recursive procedure) "\n")
                 (error procedure "apply: unknown procedure type"))]))

 (define (apply procedure arguments recur source)
   (let ([dispatch-fn (apply-dispatch-fn procedure)])
     (dispatch-fn procedure arguments recur source)))

 (define (apply-compound-procedure proc args recur source)
   (eval
    (procedure-body proc)
    (extend-environment (procedure-parameters proc)
                        args
                        (procedure-environment proc))
    recur
    source))


 ;; --------------------------------------------------------------------
 ;; Procedure data type (compressed)

 (define (make-procedure parameters &body &env)
   (&list (compress-symbol 'procedure)
          (compress-recursive parameters)
          &body
          &env))

 (define (compound-procedure? &p)
   (&tagged-list? &p 'procedure))

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
           (map (compose compress-recursive cdr) primitive-constants)))


 ;; --------------------------------------------------------------------
 ;; Deterministic primitive procedures (compressed)

 (define (deterministic-primitive-procedure? &proc)
   (&tagged-list? &proc 'primitive))

 (define (apply-deterministic-primitive-procedure &proc args recur source)
   (compress-recursive
    (scheme-apply (primitive-implementation &proc)
                  (map &expand-recursive args))))


 ;; --------------------------------------------------------------------
 ;; Stochastic primitive procedures (compressed)

 (define (stochastic-primitive-procedure? &proc)
   (&tagged-list? &proc 'rand))

 (define (apply-stochastic-primitive-procedure &proc args recur source)
   (compress-recursive
    (scheme-apply (primitive-implementation &proc)
                  (cons source (map &expand-recursive args)))))


 ;; --------------------------------------------------------------------
 ;; Top-level

 (define (setup-environment)
   (extend-environment (all-primitive-names)
                       (all-primitive-objects)
                       the-empty-environment))

 (define default-source
   (lambda (p) (< (random-real) p)))

 (define (make-default-recur source)
   (letrec ([recur (lambda (syntax env)
                     (begin
                       (assert (syntax? syntax))
                       (eval syntax env recur source)))])
     recur))

 (define (interpreter expr recur source)
   (let ([env (setup-environment)])
     (eval (sexpr->syntax expr env) env recur source)))

 (define (sicp-interpreter expr)
   (let ([env (setup-environment)]
         [recur (make-default-recur default-source)])
     (&expand-recursive (recur (sexpr->syntax expr env) env))))

 )