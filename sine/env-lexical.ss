#!r6rs

;; based on mit-church/sicp env-lexical
;;
;; this is a version of environment handling code that uses lexical
;; addressing.

(library

 (sine env-lexical)

 (export enclosing-environment
         address-in-enclosing-environment
         first-frame
         the-empty-environment
         make-frame
         frame-variables
         frame-values
         first-frame-values
         extend-environment
         lookup-variable-value-and-id
         lookup-value-by-id
         lookup-variable-value)

 (import (rnrs)
         (scheme-tools srfi-compat :43)
         (scheme-tools srfi-compat :1))

 (define mtail cdr)
 (define mpair cons)
 (define mhead car)
 (define mlist-ref list-ref)
 (define head car)
 (define tail cdr)
 (define pair cons)

 (define (enclosing-environment env)
   (mtail env))

 ;; adjust a lexical address for passing through a lambda: pop top frame.
 ;; if frame was already 0 (first frame), return null.
 (define (address-in-enclosing-environment lexical-address)
   (if (not (pair? lexical-address)) ; if its not a lexical address, just return it...
       lexical-address
       (if (= (head lexical-address) 0)
           #f
           (pair (- (head lexical-address) 1) (tail lexical-address)))))

 (define (first-frame env) (mhead env))

 (define the-empty-environment (list)) ;; we leave this a list so that mutation fails, as it should

 (define (make-frame variables values)
   (vector (list->vector variables) (list->vector values)))

 (define (frame-variables frame) (vector-ref frame 0))

 (define (frame-values frame) (vector-ref frame 1))

 (define (first-frame-values env)
   (vector->list (frame-values (first-frame env))))

 (define (extend-environment vars vals base-env)
   (mpair (make-frame vars vals) base-env))


 ;; this returns both the value and the address as a pair (val
 ;; . addr), where address is (frame-loc . var-loc)
 (define (lookup-variable-value-and-id var top-env)
   (let ((frame-loc 0))
     (define (env-loop env)
       (define (find-var vars vals)
         (let ((var-index (vector-index (lambda (v) (eq? v var)) vars)))
           (if (not var-index)
               (begin (set! frame-loc (+ frame-loc 1))
                      (env-loop (enclosing-environment env)))
               (pair (vector-ref vals var-index) (pair frame-loc var-index)))))
       (if (eq? env the-empty-environment)
           (raise-continuable "Unbound variable")
           (let ((frame (first-frame env)))
             (find-var (frame-variables frame)
                       (frame-values frame) ))))
     (env-loop top-env) ))

 ;; this uses a lexical address to get a value in an environment,
 ;; returns both the value and the address
 (define (lookup-value-by-id address env)
   (pair (vector-ref (frame-values (mlist-ref env (head address)))
                     (tail address))
         address))

 (define (lookup-variable-value var env)
   (let ((value-and-id (lookup-variable-value-and-id var env)))
     (head value-and-id)))
 ;; (if (fail? value-and-id) 'fail (head value-and-id))))


 ;; below here are the parts that use mutation (used only for define)

 ;; (define (add-binding-to-frame! var val frame)
 ;;   (vector-set! frame 0 (vector-append (vector var) (frame-variables frame)))
 ;;   (vector-set! frame 1 (vector-append (vector val) (frame-values frame))))

 ;; this is used in trace-eval.ss to implement define..
 ;; (define (define-variable! var val env)
 ;;   (let* ((frame (first-frame env))
 ;;          (var-index (vector-index (lambda (v) (eq? v var)) (frame-variables frame))))
 ;;     (if (not var-index)
 ;;         (add-binding-to-frame! var val frame)
 ;;         (vector-set! (frame-values frame) var-index val))))

 ;; ;; this one is used only in trace-constraint-propagation.ss
 ;; (define (set-variable-value! var val env)
 ;;   (define (env-loop env)
 ;;     (define (set-var vars vals)
 ;;       (let ((var-index (vector-index (lambda (v) (eq? v var)) vars)))
 ;;         (if (not var-index);ie. (#f? var-index)
 ;;             (env-loop (enclosing-environment env))
 ;;             (vector-set! vals var-index val))))
 ;;     (if (eq? env the-empty-environment)
 ;;         (error "Unbound variable -- SET!" var)
 ;;         (let ((frame (first-frame env)))
 ;;           (set-var (frame-variables frame)
 ;;                    (frame-values frame)))))
 ;;   (env-loop env))


 ;; tools for checking environment equivalence relative to a set of
 ;; bindings
 ;;
 ;; relevant-bindings should be a list of lists the same
 ;; length as new-env, each of which is the index of variable bindings
 ;; to pay attention to.
 ;; (define (relative-env-equiv new-env old-env relevant-bindings)
 ;;   (if (or (null? relevant-bindings) (eq? new-env old-env))
 ;;       #t
 ;;       (if (or (eq? new-env the-empty-environment) (eq? old-env the-empty-environment))
 ;;           #f
 ;;           (if (frames-equiv? (frame-values (first-frame new-env))
 ;;                              (frame-values (first-frame old-env))
 ;;                              (frame-unchanged (first-frame new-env))
 ;;                              (first relevant-bindings))
 ;;               (relative-env-equiv (enclosing-environment new-env)
 ;;                                   (enclosing-environment old-env)
 ;;                                   (tail relevant-bindings))
 ;;               #f))))

 ;; set this to #f for less fine-grained, but possibly faster eq?-based env-equiv.
 ;; (define CHECK-VALUES-WITH-EQUAL #t)

 ;; (define (frames-equiv? new-frame-values old-frame-values unchanged relevant-indices)
 ;;   (if (null? relevant-indices)
 ;;       #t
 ;;       (if (or (vector-ref unchanged (first relevant-indices)) ;;this uses binding-time eq check....
 ;;               (if CHECK-VALUES-WITH-EQUAL
 ;;                   (equal? (vector-ref new-frame-values (first relevant-indices))
 ;;                           (vector-ref old-frame-values (first relevant-indices)))
 ;;                   #f)
 ;;               )
 ;;           (frames-equiv? new-frame-values old-frame-values unchanged (tail relevant-indices))
 ;;           #f)))


 )