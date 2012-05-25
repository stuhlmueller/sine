#!r6rs

;; Incremental sampler using subproblems

(import (rnrs)
        (sine coroutine-interpreter)
        (sine preamble)
        (sine value-number)
        (sine hashtable)
        (sine syntax)
        (sine desugar)
        (sine delimcc-simple-r6rs)
        (scheme-tools srfi-compat :1)
        (scheme-tools srfi-compat :43)
        (scheme-tools math)
        (scheme-tools))


;; --------------------------------------------------------------------
;; Utils

(define (vector-sum vec)
  (let ([total 0])
    (vector-for-each (lambda (n) (set! total (+ total n))) vec)
    total))

(define (normalize-vector vec)
  (let ([s (vector-sum vec)])
    (vector-map (lambda (x) (/ x s))
                vec)))

(define (apply-recur recur)
  (let ([syntax+env (&expand-pair (recur-state recur))])
    (apply (recur-call recur)
           (list (car syntax+env)
                 (cdr syntax+env)))))


;; --------------------------------------------------------------------
;; Data structure for marginals

(define marginal-unknown (gensym 'marginal-unknown))

(define (marginal-unknown? marginal)
  (eq? marginal marginal-unknown))

(define (make-empty-marginal)
  (make-eq-hashtable))

(define (make-marginal vals probs)
  (let ([marginal (make-empty-marginal)])
    (vector-for-each (lambda (val prob) (hashtable-set! marginal val prob))
                     vals
                     probs)
    marginal))

(define (marginal-vals marginal)
  (hashtable-keys marginal))

(define (marginal-ps marginal)
  (hashtable-values marginal))

(define (marginal-vals&ps marginal)
  (hashtable-entries marginal))

(define (map-over-marginal-vector proc marginal)
  (let-values ([(vals ps) (marginal-vals&ps marginal)])
    (vector-map proc vals ps)))

(define (map-over-marginal proc marginal)
  (let-values ([(vals ps) (marginal-vals&ps marginal)])
    (make-marginal vals
                   (vector-map proc vals ps))))

(define (sample-marginal marginal)
  (let-values ([(vals ps) (marginal-vals&ps marginal)]) ;; avoid sampling if p=1?
    (multinomial (vector->list vals)
                 (vector->list ps)))) ;; multinomial renormalizes

(define (get-marginal-p marginal val)
  (hashtable-ref/default marginal val (lambda () 0.0)))

(define (set-marginal-p! marginal val p)
  (hashtable-set! marginal val p))

(define (marginal-mass marginal)
  (vector-sum (marginal-ps marginal)))

(define (marginal-diff local-marginal global-marginal)
  (cond [(marginal-unknown? global-marginal) (make-empty-marginal)] ;; could avoid creation
        [(marginal-unknown? local-marginal) global-marginal]
        [else (map-over-marginal (lambda (val global-p)
                                   (let ([local-p (get-marginal-p local-marginal val)])
                                     (- global-p local-p)))
                                 global-marginal)]))

(define (reweight-marginal marginal weights)
  (make-marginal (marginal-vals marginal)
                 (normalize-vector
                  (vector-map *
                              (marginal-ps marginal)
                              weights))))

(define (print-marginal marginal id)
  (pe id ":  ")
  (map-over-marginal-vector (lambda (v p) (pe v ", " p ";  "))
                            marginal)
  (pe "\n"))


;; --------------------------------------------------------------------
;; DP tables


;; Global marginals

(define global-marginals
  (make-eq-hashtable))

(define (get-global-marginal &state)
  (hashtable-ref/default global-marginals
                         &state
                         (lambda () (make-empty-marginal)))) ;; inefficient?

(define (set-global-marginal! &state marginal)
  (hashtable-set! global-marginals
                  &state
                  marginal))

(define (increment-global-marginal! &state v p)
  (let ([marginal (get-global-marginal &state)])
    (if (marginal-unknown? marginal)
        (set-global-marginal! &state
                              (make-marginal (vector v)
                                             (vector p)))
        (set-marginal-p! marginal v p)))) ;; mutation!


;; Local marginals

(define local-marginals
  (make-eq-hashtable))

(define (get-local-marginal &state &slot)
  (hashtable-ref/default local-marginals
                         (&cons &state &slot)
                         (lambda () (make-empty-marginal)))) ;; inefficient?

(define (set-local-marginal! &state &slot marginal)
  (hashtable-set! local-marginals
                  (&cons &state &slot)
                  marginal))


;; Visited probability mass (local)

(define visited-mass-table
  (make-eq-hashtable))

(define (get-visited-mass &state &slot)
  (hashtable-ref visited-mass-table
                 (&cons &state &slot)
                 0.0))

(define (set-visited-mass! &state &slot mass)
  (hashtable-set! visited-mass-table
                  (&cons &state &slot)
                  mass))

;; Improve this? (use better check than visited mass being 0)
(define (update-visited-mass! &state &slot)
  (when (not (&null? &slot))
        (let ([marginal (get-local-marginal &state &slot)])
          (let ([visited-mass (vector-sum
                               (map-over-marginal-vector
                                (lambda (&v p) (* p (get-visited-mass &state (&cons &v &slot))))
                                marginal))])
            (if (= visited-mass 0.0)
                (set-visited-mass! &state &slot (marginal-mass marginal))
                (set-visited-mass! &state &slot visited-mass))
            (update-visited-mass! &state (&cdr &slot))))))



;; --------------------------------------------------------------------
;; Debug tools

(define (recur->string recur)
  (if (&pair? (recur-state recur))
      (->string:n (syntax->original-expr (&car (recur-state recur))) 80)
      (->string:n (&expand-recursive (recur-state recur)) 80)))

(define (show-stack stack)
  (pe "stack:\n")
  (for-each (lambda (e i)
              (pe "  " i ": " (recur->string e) "\n"))
            stack
            (reverse (iota (length stack))))
  (pe "\n"))

(define (show-state state)
  (pe "state:\n")
  (cond [(recur? state) (pe "  recur: " (recur->string state))]
        [(terminal? state) (pe "  term: " (->string:n (&expand-recursive (terminal-value state)) 80))]
        [(xrp? state) (pe "  xrp call: " (vector-map &expand-recursive (xrp-vals state)) " " (xrp-probs state))]
        [else (pe state)])
  (pe "\n"))

(define (&->string:n v n)
  (->string:n (&expand-recursive v) n))


;; --------------------------------------------------------------------
;; Updates
;;
;; Q: is it valid to enqueue updates?

(define update-queue '())

(define (enqueue-update! call . args)
  (set! update-queue
        (cons (cons call args)
              update-queue)))

(define (show-update-queue)
  (for-each pretty-print
            (reverse update-queue)))


;; --------------------------------------------------------------------
;; Sampler

(define top-recur
  (make-recur 'no-cont 'no-call 'top))

(define (sample-top state)
  (let-values ([(&value score &slot) (sample state (list top-recur) &null 1.0)])
    (let ([value (&expand-recursive &value)])
      (pe "value: " value "\n"
          "score: " score "\n"
          "slot: " (&expand-recursive &slot) "\n")
      value)))

(define (sample state stack &slot score)
  (show-state state)
  (show-stack stack)
  (cond [(recur? state) (sample-recur state stack &slot score)]
        [(xrp? state) (sample-xrp state stack &slot score)]
        [(terminal? state) (sample-terminal state stack &slot score)]
        [else (error state "unknown state type")]))

(define (sample-xrp xrp stack &slot score)
  (let ([parent-recur (car stack)])
    (pe "at xrp, setting marginal for " (recur->string parent-recur) " to "
        (vector-map &expand-recursive (xrp-vals xrp)) ", "(xrp-probs xrp) "\n\n")
    (set-global-marginal! (recur-state parent-recur)
                          (make-marginal (xrp-vals xrp)
                                         (xrp-probs xrp)))
    (sample-recur (make-recur (lambda (x) (make-terminal x))
                              (recur-call parent-recur)
                              (recur-state parent-recur))
                  (cdr stack)
                  &slot
                  score)))

(define (sample-recur recur stack &slot score)
  (let* ([&state (recur-state recur)]
         [local-marginal (get-local-marginal (recur-state (car stack))
                                             &slot)]
         [global-marginal (get-global-marginal &state)]
         [inc-marginal (marginal-diff local-marginal global-marginal)]
         [use-marginal (flip (marginal-mass inc-marginal))])

    (let-values ([(value new-score)
                  (if use-marginal
                      (let* ([unvisited-masses
                              (vector-map
                               (lambda (&v) (- 1.0 (get-visited-mass (recur-state (car stack))
                                                                (&cons &v &slot))))
                               (marginal-vals inc-marginal))]
                             [marginal (reweight-marginal inc-marginal
                                                          (normalize-vector unvisited-masses))]
                             [v (sample-marginal marginal)]
                             [p (get-marginal-p inc-marginal v)]) ;; Q: what marginal to use here?
                        (pe "sampled " (&expand-recursive v) " from marginal of "
                            (recur->string recur) "\n\n")
                        (values v p))
                      (let-values ([(v p &s) (sample (reset (make-terminal (apply-recur recur)))
                                                     (cons recur stack)
                                                     &null
                                                     1.0)])
                        (pe "return " (&->string:n v 60) "\nfrom " (recur->string recur) "\n\n")
                        (enqueue-update! 'increment-global-marginal! &state v p)
                        (enqueue-update! 'update-visited-mass! &state &s)
                        (values v p)))])

      (enqueue-update! 'set-local-marginal!
                       (recur-state (car stack))
                       &slot
                       (get-global-marginal &state))

      (sample ((recur-cont recur) value)
              stack
              (&cons value &slot)
              (* score new-score)))))

(define (sample-terminal terminal stack &slot score)
  (values (terminal-value terminal)
          score
          (&list (terminal-value terminal))))



;; --------------------------------------------------------------------
;; Test

(define (test)
  (let ([expr (with-preamble '(list (flip) (flip)))])
    (pe "original expr:\n")
    (pretty-print expr)
    (pe "\n")
    (pe "desugared expr:\n")
    (pretty-print (desugar-all expr))
    (pe "\n")
    (pe (sample-top (coroutine-interpreter expr))
        "\n")
    (pe "\nEnqueued updates:\n")
    (show-update-queue)))

(test)