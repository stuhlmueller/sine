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

(define (set-marginal! table &args marginal)
  (hashtable-set! table &args marginal))

(define (get-marginal table &args)
  (hashtable-ref/default table
                         &args
                         (lambda ()
                           (let ([marginal (make-empty-dist)])
                             (set-marginal! table &args marginal)
                             marginal))))

(define (increment-marginal! table &args v p)
  (let ([marginal (get-marginal table &args)])
    (let ([new-p (logsumexp (get-dist-prob marginal v) p)])
      (assert (<= new-p LOG-PROB-1))
      (set-dist-prob! marginal v new-p)
      (assert* (<= (dist-mass marginal) (+ LOG-PROB-1 .1))
               (lambda () (pen (exp (dist-mass marginal)))))
      marginal)))

(define global-marginals
  (make-eq-hashtable))

(define (get-global-marginal &args)
  (get-marginal global-marginals &args))

(define (set-global-marginal! &args dist)
  (set-marginal! global-marginals &args dist))

(define (increment-global-marginal! &args v p)
  (increment-marginal! global-marginals &args v p))

(define local-marginals
  (make-eq-hashtable))

(define (get-local-marginal &args &slot)
  (get-marginal local-marginals (&cons &args &slot)))

(define (set-local-marginal! &args &slot dist)
  (set-marginal! local-marginals (&cons &args &slot) dist))

(define (increment-local-marginal! parent-subcall &slot v p)
  (increment-marginal! local-marginals (&cons (subcall-args parent-subcall) &slot) v p))


;; Explored probability mass (local)

(define explored-mass-table
  (make-eq-hashtable))

(define (get-explored-mass &args &slot)
  (hashtable-ref explored-mass-table
                 (&cons &args &slot)
                 LOG-PROB-0))

(define (fully-explored? &args &slot)
  (= (get-explored-mass &args &slot) LOG-PROB-1))

(define (set-explored-mass! &args &slot mass)
  (hashtable-set! explored-mass-table
                  (&cons &args &slot)
                  mass))

(define (get-unexplored-mass parent-args &slot vals)
  (vector-map (lambda (&v) (begin
                        ;; (pen "get-unexplored-mass: " (subcall-args parent-subcall) " " (slot->string (&cons &v &slot)))
                        (log1minus (get-explored-mass parent-args
                                                      (&cons &v &slot)))))
              vals))

(define (update-explored-mass! &args &slot)
  ;; (pen "updating " &args " " (slot->string &slot))
  (when (not (fully-explored? &args &slot))
        (let*-values ([(marginal) (get-local-marginal &args &slot)]
                      [(vals ps) (dist-vals&ps marginal)])
          ;; (pen "-- " &args " " (&expand-recursive &slot) " " vals " " ps)
          (when (not (= (vector-length vals) 0))
                (let ([weighted-child-mass
                       (apply logsumexp
                              (vector->list
                               (vector-map (lambda (&v p) (+ p (get-explored-mass &args (&cons &v &slot))))
                                           vals ps)))])
                  (set-explored-mass! &args &slot weighted-child-mass)
                  (assert (not (= (get-explored-mass &args &slot) LOG-PROB-0)))))))
  (when (not (&expand-boolean (&null? &slot)))
        (update-explored-mass! &args (&cdr &slot))))

(define (reweight-dist dist log-weights)
  (make-dist (dist-vals dist)
             (vector-map +
                         (dist-ps dist)
                         log-weights)))

(define (reweight/unexplored &args &slot dist)
  (if (eq? &args 'top)
      dist
      (let ([unexplored-mass (get-unexplored-mass &args &slot (dist-vals dist))])
        ;; (pen "vals: " (dist-vals dist))
        ;; (pen "unexplored-mass: " (vector-map exp unexplored-mass))
        (reweight-dist dist unexplored-mass))))



;; --------------------------------------------------------------------
;; Sampler

(define top-subcall
  (make-subcall 'top-cont 'top-call 'top))

(define (sample-top state)
  (let ([init-stack (list top-subcall)]
        [init-slot &null]
        [init-score LOG-PROB-1]
        [init-path '()])
    (assert (subcall? state))
    (let-values ([(&value score &slot path) (sample state init-stack init-slot init-score init-path)])
      (&expand-recursive &value))))

(define (sample state stack &slot score path)
  (cond [(terminal? state) (sample-terminal state stack &slot score path)]
        [(xrp? state) (sample-xrp state stack &slot score path)]
        [(subcall? state) (sample-subcall state stack &slot score path)]
        [else (error state "unknown state type")]))

(define (sample-terminal terminal stack &slot score path)
  (values (terminal-value terminal)
          score
          &slot
          (cons (list terminal (terminal-value terminal) LOG-PROB-1) path)))

(define (sample-xrp xrp stack &slot score path)
  (let* ([xrp-dist (make-dist (xrp-vals xrp) (xrp-probs xrp))]
         [reweighted-dist (reweight/unexplored (subcall-args (car stack)) &slot xrp-dist)]
         [value (sample-dist reweighted-dist)]
         [new-score (get-dist-prob xrp-dist value)])
    (sample ((xrp-cont xrp) value)
            stack
            (&cons value &slot)
            (+ score new-score)
            (cons (list xrp value new-score) path))))

(define (sample-subcall subcall stack &slot score path)
  (let* ([global-marginal (get-global-marginal (subcall-args subcall))]
         [reweighted-marginal (reweight/unexplored (subcall-args (car stack)) &slot global-marginal)]
         [use-marginal (log-flip (dist-mass reweighted-marginal))])
    (if use-marginal
        (sample-subcall-marginal subcall stack &slot score global-marginal reweighted-marginal path)
        (sample-subcall-internally subcall stack &slot score path))))

(define (sample-subcall-marginal subcall stack &slot score global-marginal reweighted-marginal path)
  (let* ([value (sample-dist reweighted-marginal)]
         [new-score (get-dist-prob global-marginal value)])
    (sample ((subcall-cont subcall) value)
            stack
            (&cons value &slot)
            (+ score new-score)
            (cons (list subcall value new-score) path))))

(define (sample-subcall-internally subcall stack &slot score path)
  (let-values ([(&value new-score &internal-slot internal-path)
                (sample (reset (make-terminal (apply-subcall subcall)))
                        (cons subcall stack)
                        &null
                        LOG-PROB-1
                        '())])
    (sample ((subcall-cont subcall) &value)
            stack
            (&cons &value &slot)
            (+ score new-score)
            (cons (list subcall &value new-score (reverse internal-path)) path))))


;; --------------------------------------------------------------------
;; Updating

;; (define (update! path)
;;   ;; make a hashtable that stores for each subproblem (1) a *set* of new paths, (2) local id
;;   ;; update global and local marginals
;;   ;; set explored mass for terminals
;;   ;; in reverse order (globally and for each subproblem), inside to outside, update explored mass
;;   ...)

;; terminal:
;; (set-explored-mass! (subcall-args (car stack)) &slot LOG-PROB-1)

;; subcall:
;; (set-local-marginal! (subcall-args (car stack)) &slot global-marginal)

;; xrp:
;; (set-local-marginal! (subcall-args (car stack)) &slot xrp-dist)

;; internal subcall:
;; (update-explored-mass! (subcall-args subcall) &internal-slot)
;; (let ([new-global-marginal (increment-global-marginal! (subcall-args subcall) &value new-score)])
;;   (set-local-marginal! (subcall-args (car stack)) &slot new-global-marginal))

;; --------------------------------------------------------------------
;; Test

(define (test)
  (let ([expr '(cache (list (flip .6) (flip .6)))])
    (let ([v (sample-top (coroutine-interpreter expr))])
      (pen " " v)
      ;; (pe "\nExplored mass:\n")
      ;; (hashtable-for-each (lambda (k v) (pen (&->string:n k 30) ": " (exp v))) explored-mass-table)
      ;; (pe "\n\n")
      v)))

(display (repeat 40 test))