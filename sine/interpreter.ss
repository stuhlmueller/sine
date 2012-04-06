#!r6rs

;; Based on metacircular interpreter in SICP.
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
                 (apply apply-in-underlying-scheme))
         (scheme-tools srfi-compat :1)
         (only (scheme-tools math) random-real))

 (define true #t)
 
 (define false #f)

 (define (eval exp env recur source)
   (cond ((self-evaluating? exp) exp)
         ((variable? exp) (lookup-variable-value exp env))
         ((quoted? exp) (text-of-quotation exp))
         ((assignment? exp) (eval-assignment exp env recur))
         ((definition? exp) (eval-definition exp env recur))
         ((if? exp) (eval-if exp env recur))
         ((lambda? exp)
          (make-procedure (lambda-parameters exp)
                          (lambda-body exp)
                          env))
         ((begin? exp) 
          (eval-sequence (begin-actions exp) env recur))
         ((cond? exp) (recur (cond->if exp) env))
         ((application? exp)
          (apply (recur (operator exp) env)
                 (list-of-values (operands exp) env recur)
                 recur
                 source))
         (else
          (error "Unknown expression type -- EVAL" exp))))

 (define (apply procedure arguments recur source)
   (cond ((primitive-procedure? procedure)
          (apply-primitive-procedure procedure arguments))
         ((random-primitive-procedure? procedure)
          (apply-random-primitive-procedure procedure arguments source))
         ((compound-procedure? procedure)
          (eval-sequence
           (procedure-body procedure)
           (extend-environment
            (procedure-parameters procedure)
            arguments
            (procedure-environment procedure))
           recur))
         (else
          (error
           "Unknown procedure type -- APPLY" procedure))))

 (define (list-of-values exps env recur)
   (if (no-operands? exps)
       '()
       (cons (recur (first-operand exps) env)
             (list-of-values (rest-operands exps) env recur))))

 (define (eval-if exp env recur)
   (if (true? (recur (if-predicate exp) env))
       (recur (if-consequent exp) env)
       (recur (if-alternative exp) env)))

 (define (eval-sequence exps env recur)
   (cond ((last-exp? exps) (recur (first-exp exps) env))
         (else (recur (first-exp exps) env)
               (eval-sequence (rest-exps exps) env recur))))

 (define (eval-assignment exp env recur)
   (set-variable-value! (assignment-variable exp)
                        (recur (assignment-value exp) env)
                        env)
   'ok)

 (define (eval-definition exp env recur)
   (define-variable! (definition-variable exp)
     (recur (definition-value exp) env)
     env)
   'ok)

 
 ;; SECTION 4.1.2

 (define (self-evaluating? exp)
   (cond ((number? exp) true)
         ((string? exp) true)
         (else false)))

 (define (quoted? exp)
   (tagged-list? exp 'quote))

 (define (text-of-quotation exp) (cadr exp))

 (define (tagged-list? exp tag)
   (if (pair? exp)
       (eq? (car exp) tag)
       false))

 (define (variable? exp) (symbol? exp))

 (define (assignment? exp)
   (tagged-list? exp 'set!))

 (define (assignment-variable exp) (cadr exp))

 (define (assignment-value exp) (caddr exp))


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
 (define (lambda-body exp) (cddr exp))

 (define (make-lambda parameters body)
   (cons 'lambda (cons parameters body)))


 (define (if? exp) (tagged-list? exp 'if))

 (define (if-predicate exp) (cadr exp))

 (define (if-consequent exp) (caddr exp))

 (define (if-alternative exp)
   (if (not (null? (cdddr exp)))
       (cadddr exp)
       'false))

 (define (make-if predicate consequent alternative)
   (list 'if predicate consequent alternative))


 (define (begin? exp) (tagged-list? exp 'begin))

 (define (begin-actions exp) (cdr exp))

 (define (last-exp? seq) (null? (cdr seq)))
 (define (first-exp seq) (car seq))
 (define (rest-exps seq) (cdr seq))

 (define (sequence->exp seq)
   (cond ((null? seq) seq)
         ((last-exp? seq) (first-exp seq))
         (else (make-begin seq))))

 (define (make-begin seq) (cons 'begin seq))


 (define (application? exp) (pair? exp))
 (define (operator exp) (car exp))
 (define (operands exp) (cdr exp))

 (define (no-operands? ops) (null? ops))
 (define (first-operand ops) (car ops))
 (define (rest-operands ops) (cdr ops))


 (define (cond? exp) (tagged-list? exp 'cond))

 (define (cond-clauses exp) (cdr exp))

 (define (cond-else-clause? clause)
   (eq? (cond-predicate clause) 'else))

 (define (cond-predicate clause) (car clause))

 (define (cond-actions clause) (cdr clause))

 (define (cond->if exp)
   (expand-clauses (cond-clauses exp)))

 (define (expand-clauses clauses)
   (if (null? clauses)
       'false                          ; no else clause
       (let ((first (car clauses))
             (rest (cdr clauses)))
         (if (cond-else-clause? first)
             (if (null? rest)
                 (sequence->exp (cond-actions first))
                 (error "ELSE clause isn't last -- COND->IF"
                        clauses))
             (make-if (cond-predicate first)
                      (sequence->exp (cond-actions first))
                      (expand-clauses rest))))))

 
 ;; SECTION 4.1.3

 (define (true? x)
   (not (eq? x false)))

 (define (false? x)
   (eq? x false))


 (define (make-procedure parameters body env)
   (list 'procedure parameters body env))

 (define (compound-procedure? p)
   (tagged-list? p 'procedure))


 (define (procedure-parameters p) (cadr p))
 (define (procedure-body p) (caddr p))
 (define (procedure-environment p) (cadddr p))


 (define (enclosing-environment env) (cdr env))

 (define (first-frame env) (car env))

 (define the-empty-environment '())

 (define (make-frame variables values)
   (cons variables values))

 (define (frame-variables frame) (car frame))
 (define (frame-values frame) (cdr frame))

 (define (add-binding-to-frame! var val frame)
   (set-car! frame (cons var (car frame)))
   (set-cdr! frame (cons val (cdr frame))))

 (define (extend-environment vars vals base-env)
   (if (= (length vars) (length vals))
       (cons (make-frame vars vals) base-env)
       (if (< (length vars) (length vals))
           (error "Too many arguments supplied" vars vals)
           (error "Too few arguments supplied" vars vals))))

 (define (lookup-variable-value var env)
   (define (env-loop env)
     (define (scan vars vals)
       (cond ((null? vars)
              (env-loop (enclosing-environment env)))
             ((eq? var (car vars))
              (car vals))
             (else (scan (cdr vars) (cdr vals)))))
     (if (eq? env the-empty-environment)
         (error "Unbound variable" var)
         (let ((frame (first-frame env)))
           (scan (frame-variables frame)
                 (frame-values frame)))))
   (env-loop env))

 (define (set-variable-value! var val env)
   (define (env-loop env)
     (define (scan vars vals)
       (cond ((null? vars)
              (env-loop (enclosing-environment env)))
             ((eq? var (car vars))
              (set-car! vals val))
             (else (scan (cdr vars) (cdr vals)))))
     (if (eq? env the-empty-environment)
         (error "Unbound variable -- SET!" var)
         (let ((frame (first-frame env)))
           (scan (frame-variables frame)
                 (frame-values frame)))))
   (env-loop env))

 (define (define-variable! var val env)
   (let ((frame (first-frame env)))
     (define (scan vars vals)
       (cond ((null? vars)
              (add-binding-to-frame! var val frame))
             ((eq? var (car vars))
              (set-car! vals val))
             (else (scan (cdr vars) (cdr vals)))))
     (scan (frame-variables frame)
           (frame-values frame))))

 
 ;; SECTION 4.1.4

 (define (primitive-procedure? proc)
   (tagged-list? proc 'primitive))

 (define (primitive-implementation proc) (cadr proc))

 (define primitive-procedures
   (list (list 'car car)
         (list 'cdr cdr)
         (list 'cons cons)
         (list 'null? null?)
         (list 'list list)
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

 (define (apply-primitive-procedure proc args)
   (apply-in-underlying-scheme
    (primitive-implementation proc) args))


 ;; RANDOM PRIMITIVE PROCEDURES

 (define (apply-random-primitive-procedure proc args source)
   (apply-in-underlying-scheme (random-primitive-implementation proc)
                               (cons source args)))

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
 

 ;; INTERPRETERS

 (define (setup-environment)
   (let ((initial-env
          (extend-environment (append (primitive-procedure-names)
                                      (random-primitive-procedure-names))
                              (append (primitive-procedure-objects)
                                      (random-primitive-procedure-objects))
                              the-empty-environment)))
     (define-variable! 'true true initial-env)
     (define-variable! 'false false initial-env)
     initial-env))

 (define default-source 
   (lambda (p) (< (random-real) p)))

 (define (make-default-recur source)
   (letrec ([recur (lambda (expr env) (eval expr env recur source))])
     recur))
 
 (define (interpreter expr recur source)
   (let ([env (setup-environment)])
     (eval expr env recur source)))

 (define (sicp-interpreter expr)
   (let ([env (setup-environment)]
         [recur (make-default-recur default-source)])
     (recur expr env)))

 )