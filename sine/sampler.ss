#!r6rs

;; Incremental sampler using subproblems

(import (rnrs)
        (scheme-tools hashtable)
        (scheme-tools math distributions)
        (scheme-tools math)
        (scheme-tools srfi-compat :1)
        (scheme-tools srfi-compat :43)
        (scheme-tools value-number)
        (scheme-tools)
        (sine coroutine-interpreter)
        (sine debug)
        (sine delimcc-simple-r6rs)
        (sine desugar)
        (sine preamble)
        (sine syntax)
        (sine utils))



;; --------------------------------------------------------------------
;; DP tables


;; Global and local marginals

(define (set-marginal! table &state marginal)
  (hashtable-set! table &state marginal))

(define (get-marginal table &state)
  (hashtable-ref/default table
                         &state
                         (lambda ()
                           (let ([marginal (make-empty-dist)])
                             (set-marginal! table &state marginal)
                             marginal))))

(define (increment-marginal! table &state v p)
  (let ([marginal (get-marginal table &state)])
    (let ([new-p (logsumexp (get-dist-prob marginal v) p)])
      (assert (<= new-p LOG-PROB-1))
      (set-dist-prob! marginal v new-p)
      (assert (<= (dist-mass marginal) LOG-PROB-1)))))

(define global-marginals
  (make-eq-hashtable))

(define (get-global-marginal &state)
  (get-marginal global-marginals &state))

(define (set-global-marginal! &state dist)
  (set-marginal! global-marginals &state dist))

(define (increment-global-marginal! &state v p)
  (increment-marginal! global-marginals &state v p))

(define local-marginals
  (make-eq-hashtable))

(define (get-local-marginal &state &slot)
  (get-marginal local-marginals (&cons &state &slot)))

(define (set-local-marginal! &state &slot dist)
  (set-marginal! local-marginals (&cons &state &slot) dist))

(define (increment-local-marginal! parent-recur &slot v p)
  (increment-marginal! local-marginals (&cons (recur-state parent-recur) &slot) v p))



;; Explored probability mass (local)

(define explored-mass-table
  (make-eq-hashtable))

(define (get-explored-mass &state &slot)
  (hashtable-ref explored-mass-table
                 (&cons &state &slot)
                 LOG-PROB-0))

(define (fully-explored? &state &slot)
  (= (get-explored-mass &state &slot) LOG-PROB-1))

(define (set-explored-mass! &state &slot mass)
  (hashtable-set! explored-mass-table
                  (&cons &state &slot)
                  mass))

(define (get-unexplored-mass parent-recur &slot vals)
  (vector-map (lambda (&v) (log1minus (get-explored-mass (recur-state parent-recur)
                                                    (&cons &v &slot))))
              vals))

(define (update-explored-mass! &state &slot)
  (pen "updating " &state " " (&expand-recursive &slot))
  (when (not (fully-explored? &state &slot))
        (let*-values ([(marginal) (get-local-marginal &state &slot)]
                      [(vals ps) (dist-vals&ps marginal)])
          (pen "-- " &state " " (&expand-recursive &slot) " " vals " " ps)
          (when (not (= (vector-length vals) 0))
                (let ([weighted-child-mass
                       (apply logsumexp
                              (vector->list
                               (vector-map (lambda (&v p) (+ p (get-explored-mass &state (&cons &v &slot))))
                                           vals ps)))])
                  (set-explored-mass! &state &slot weighted-child-mass)
                  (assert (not (= (get-explored-mass &state &slot) LOG-PROB-0)))))))
  (when (not (&expand-boolean (&null? &slot)))
        (update-explored-mass! &state (&cdr &slot))))

(define (reweight-dist dist log-weights)
  (make-dist (dist-vals dist)
             (vector-map +
                         (dist-ps dist)
                         log-weights)))

(define (reweight/unexplored state &slot dist)
  (let ([unexplored-mass (get-unexplored-mass state &slot (dist-vals dist))])
    (reweight-dist dist unexplored-mass)))



;; --------------------------------------------------------------------
;; Sampler

(define top-recur
  (make-recur 'top-cont 'top-call 'top))

(define (sample-top state)
  (let ([init-stack (list top-recur)]
        [init-slot &null]
        [init-score LOG-PROB-1])
    (let-values ([(&value score &internal-slot) (sample state init-stack init-slot init-score)])
      (update-explored-mass! (recur-state top-recur) &internal-slot)
      (pen score)
      (&expand-recursive &value))))

(define (sample state stack &slot score)
  (cond [(terminal? state) (sample-terminal state stack &slot score)]
        [(xrp? state) (sample-xrp state stack &slot score)]
        [else (error state "unknown state type")]))

(define (sample-terminal terminal stack &slot score)
  (set-explored-mass! (recur-state (car stack)) &slot LOG-PROB-1)
  (values (terminal-value terminal)
          score
          &slot))

(define (sample-xrp xrp stack &slot score)
  (let* ([xrp-dist (make-dist (xrp-vals xrp) (xrp-probs xrp))]
         [reweighted-dist (reweight/unexplored (car stack) &slot xrp-dist)]
         [value (sample-dist reweighted-dist)]
         [new-score (get-dist-prob xrp-dist value)])
    (set-local-marginal! (recur-state (car stack)) &slot xrp-dist)
    (pen "setting local marginal " (recur-state (car stack)) " " (&expand-recursive &slot))
    (sample ((xrp-cont xrp) value)
            stack
            (&cons value &slot)
            (+ score new-score))))



;; --------------------------------------------------------------------
;; Test

(define (test)
  (let ([expr '(list (flip .99) (flip .99))])
    (pe (apply string-append (make-list 68 "-")))
    (pe "\nOriginal expr:\n")
    (pretty-print expr)
    (pe "\n")
    (pe "Desugared expr:\n")
    (pretty-print (desugar-all expr))
    (pe "\n")
    (let ([v (sample-top (coroutine-interpreter expr))])
      (pe v "\n\n")
      (pe "Explored mass:\n")
      (hashtable-for-each (lambda (k v) (pen (&expand-recursive k) ": " (exp v))) explored-mass-table)
      (pe "\n")
      v)))

(display (repeat 4 test))