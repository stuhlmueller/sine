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

(define (get-marginal-prob marginal val)
  (hashtable-ref/default marginal val (lambda () 0.0)))

(define (set-marginal-prob! marginal val p)
  (hashtable-set! marginal val p))

(define (marginal-mass marginal)
  (vector-sum (marginal-ps marginal)))

(define (get-diff-marginal local-marginal global-marginal)
  (cond [(marginal-unknown? global-marginal)
         (begin
           (assert (marginal-unknown? local-marginal))
           (make-empty-marginal))] ;; could avoid creation
        [(marginal-unknown? local-marginal) global-marginal]
        [else (map-over-marginal (lambda (val global-p)
                                   (let ([local-p (get-marginal-prob local-marginal val)])
                                     (- global-p local-p)))
                                 global-marginal)]))

(define (reweight-marginal marginal weights)
  (make-marginal (marginal-vals marginal)
                 (vector-map *
                             (marginal-ps marginal)
                             weights)))

(define (print-marginal marginal id)
  (pe id ":  ")
  (show-marginal marginal)
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
        (let ([new-p (+ (get-marginal-prob marginal v) p)])
          (assert (<= new-p 1.0))
          (set-marginal-prob! marginal
                              v
                              new-p)
          (assert* (<= (marginal-mass marginal) 1.0)
                   (lambda () (show-marginal marginal))))))) ;; mutation!


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

(define (update-local-marginal-from-global! parent-recur &slot recur)
  (set-local-marginal! (recur-state parent-recur)
                       &slot
                       (get-global-marginal (recur-state recur))))

(define (increment-local-marginal! parent-recur &slot v p)
  (let ([marginal (get-local-marginal (recur-state parent-recur) &slot)])
    (if (marginal-unknown? marginal)
        (set-local-marginal! (recur-state parent-recur)
                             (make-marginal (vector v)
                                            (vector p)))
        (let ([new-p (+ (get-marginal-prob marginal v) p)])
          (assert (<= new-p 1.0))
          (set-marginal-prob! marginal
                              v
                              new-p)
          (assert* (<= (marginal-mass marginal) 1.0)
                   (lambda () (show-marginal marginal)))))))


;; Explored probability mass (local)

(define explored-mass-table
  (make-eq-hashtable))

(define (get-explored-mass &state &slot)
  (hashtable-ref explored-mass-table
                 (&cons &state &slot)
                 0.0))

(define (set-explored-mass! &state &slot mass)
  (hashtable-set! explored-mass-table
                  (&cons &state &slot)
                  mass))

;; Improve this? (use better check than explored mass being 0)
(define (update-explored-mass! &state &slot)
  (when (not (&null? &slot))
        (let ([marginal (get-local-marginal &state &slot)])
          (let ([explored-mass (vector-sum
                                (map-over-marginal-vector
                                 (lambda (&v p) (* p (get-explored-mass &state (&cons &v &slot))))
                                 marginal))])
            (if (= explored-mass 0.0)
                (set-explored-mass! &state &slot (marginal-mass marginal))
                (set-explored-mass! &state &slot explored-mass))
            (update-explored-mass! &state (&cdr &slot))))))

(define (get-unexplored-mass parent-recur &slot vals)
  (vector-map (lambda (&v) (- 1.0 (get-explored-mass (recur-state parent-recur)
                                                (&cons &v &slot))))
              vals))


;; --------------------------------------------------------------------
;; Debug tools

;; (define (marginal->string marginal)
;;   (let-values ([(keys vals) (hashtable-entries marginal)])
;;     (->string:n (zip (vector->list keys)
;;                      (vector->list vals)) 80)))

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

(define (show-state state parent-state &slot)
  (pe "state:\n")
  (cond [(recur? state)
         (pe "  recur: " (recur->string state) "\n"
             "    global marg: " (marginal->string (get-global-marginal (recur-state state))) "\n"
             "    local marg: " (marginal->string (get-local-marginal
                                                   (recur-state parent-state) &slot)))]
        [(terminal? state) (pe "  term: " (->string:n (&expand-recursive (terminal-value state)) 80))]
        [(xrp? state) (pe "  xrp call: " (vector-map &expand-recursive (xrp-vals state)) " " (xrp-probs state))]
        [else (pe state)])
  (pe "\n"))

(define (show-slot &slot)
  (pe "slot: " (&->string:n &slot 80) "\n"))

(define (&->string:n v n)
  (->string:n (&expand-recursive v) n))

(define (marginal->string marginal)
  (apply string-append
         (vector->list
          (map-over-marginal-vector (lambda (v p) (string-append (&->string:n v 30) ", " (->string p) ";  "))
                                    marginal))))

(define (show-marginal marginal)
  (display (marginal->string marginal)))


;; --------------------------------------------------------------------
;; Updates

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
  (show-state state (car stack) &slot)
  (show-slot &slot)
  (show-stack stack)
  (cond [(recur? state) (sample-recur state stack &slot score)]
        [(xrp? state) (sample-xrp state stack &slot score)]
        [(terminal? state) (sample-terminal state stack &slot score)]
        [else (error state "unknown state type")]))

(define (sample-recur recur stack &slot score)
  (let* ([global-marginal (get-global-marginal (recur-state recur))]
         [unexplored-mass (get-unexplored-mass (car stack) &slot (marginal-vals global-marginal))]
         [reweighted-marginal (reweight-marginal global-marginal unexplored-mass)]
         [use-marginal (flip (marginal-mass reweighted-marginal))])
    (pe "global marginal: " (marginal->string global-marginal) "\n")
    (pe "unexplored mass: " unexplored-mass "\n")
    (pe "reweighted marginal: " (marginal->string reweighted-marginal) "\n")
    (let-values
        ([(value new-score)
          (if use-marginal
              (sample-recur-from-marginal recur reweighted-marginal global-marginal)
              (sample-recur-internally recur stack &slot))])
      (sample ((recur-cont recur) value)
              stack
              (&cons value &slot)
              (* score new-score)))))

(define (sample-recur-from-marginal recur reweighted-marginal original-marginal)
  (let* ([value (sample-marginal reweighted-marginal)]
         [prob (get-marginal-prob original-marginal value)])
    (pe "from marginal: " (&->string:n value 60) " of " (recur->string recur) "\n\n")
    (values value prob)))

(define (sample-recur-internally recur stack &slot)
  (let-values ([(value prob &internal-slot)
                (sample (reset (make-terminal (apply-recur recur)))
                        (cons recur stack)
                        &null
                        1.0)])
    (pe "from internal: " (&->string:n value 60) " of " (recur->string recur) "\n\n")
    (increment-global-marginal! (recur-state recur) value prob)
    (increment-local-marginal! (car stack) &slot value prob)
    (update-explored-mass! (recur-state recur) &internal-slot)
    (values value prob)))

(define (sample-xrp xrp stack &slot score)
  (let* ([global-marginal (make-marginal (xrp-vals xrp)
                                         (xrp-probs xrp))]
         [unexplored-mass (get-unexplored-mass (car stack) &slot (marginal-vals global-marginal))]
         [reweighted-marginal (reweight-marginal global-marginal unexplored-mass)]
         [value (sample-marginal reweighted-marginal)]
         [new-score (get-marginal-prob global-marginal value)])
    (pe "from xrp: " (&->string:n value 60) " of " (recur->string (car stack)) "\n\n")
    (sample ((xrp-cont xrp) value)
            stack
            (&cons value &slot)
            (* score new-score))))

(define (sample-terminal terminal stack &slot score)
  (values (terminal-value terminal)
          score
          (&list (terminal-value terminal))))



;; --------------------------------------------------------------------
;; Test

(define (test)
  (let ([expr (with-preamble '(flip))]) ;; '(list (not (flip)) (list (not (flip))))
    (pe "original expr:\n")
    (pretty-print expr)
    (pe "\n")
    (pe "desugared expr:\n")
    (pretty-print (desugar-all expr))
    (pe "\n")
    (let ([v (sample-top (coroutine-interpreter expr))])
      (pe "\n\n\n")
      v)))

(display (repeat 10 test))