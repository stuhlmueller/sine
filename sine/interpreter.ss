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
         (scheme-tools srfi-compat :1)
         (only (scheme-tools) true? false? rest pair tagged-list?)
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
   (if (true? (recur (if-syntax->if-predicate syntax) env))
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
   (cond [(primitive-procedure? procedure) apply-primitive-procedure]
         [(random-primitive-procedure? procedure) apply-random-primitive-procedure]
         [(compound-procedure? procedure) apply-compound-procedure]
         [else (error procedure "apply: unknown procedure type")]))

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

 (define (apply-primitive-procedure proc args recur source)
   (scheme-apply (primitive-implementation proc) args))

 (define (apply-random-primitive-procedure proc args recur source)
   (scheme-apply (random-primitive-implementation proc)
                 (cons source args)))


 ;; --------------------------------------------------------------------
 ;; Procedure data type

 (define (make-procedure parameters body env)
   (list 'procedure parameters body env))

 (define (compound-procedure? p)
   (tagged-list? p 'procedure))

 (define (procedure-parameters p) (cadr p))

 (define (procedure-body p) (caddr p))

 (define (procedure-environment p) (cadddr p))


 ;; --------------------------------------------------------------------
 ;; Primitive procedures

 (define (primitive-procedure? proc)
   (tagged-list? proc 'primitive))

 (define (primitive-implementation proc) (cadr proc))

 (define primitive-procedures
   (list (list 'car car)
         (list 'cdr cdr)
         (list 'cons cons)
         (list 'null? null?)
         (list 'list list)
         (list 'not not)
         (list 'or (lambda l (any (lambda (x) x) l)))
         (list 'and (lambda l (every (lambda (x) x) l)))
         (list '= =)
         (list '+ +)
         (list '- -)
         (list '* *)
         (list '/ /)))

 (define (primitive-procedure-names)
   (map car
        primitive-procedures))

 (define (primitive-procedure-objects)
   (map (lambda (proc) (list 'primitive (cadr proc)))
        primitive-procedures))


 ;; --------------------------------------------------------------------
 ;; Random primitive procedures

 (define (random-primitive-procedure? proc)
   (tagged-list? proc 'rand))

 (define (random-primitive-implementation proc)
   (cadr proc))

 (define (flip source . ?p)
   (let ([p (if (null? ?p) .5 (car ?p))])
     (let ([bit (source p)])
       bit)))

 (define random-primitive-procedures
   (list (list 'flip flip)))

 (define (random-primitive-procedure-names)
   (map car random-primitive-procedures))

 (define (random-primitive-procedure-objects)
   (map (lambda (proc) (list 'rand (cadr proc)))
        random-primitive-procedures))

 (define (all-primitive-names)
   (append (primitive-procedure-names)
           (random-primitive-procedure-names)
           (primitive-constant-names)))

 (define (all-primitive-objects)
   (append (primitive-procedure-objects)
           (random-primitive-procedure-objects)
           (primitive-constant-objects)))

 (define (primitive-constant-names)
   (list 'true 'false))

 (define (primitive-constant-objects)
   (list #t #f))


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
     (recur (sexpr->syntax expr env) env)))

 )