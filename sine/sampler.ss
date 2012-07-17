#!r6rs

;; Incremental sampler using subproblems

(import (rnrs)
        (scheme-tools hashtable)
        (scheme-tools math distributions)
        (scheme-tools math fragmented-distributions)
        (scheme-tools math)
        (scheme-tools srfi-compat :1)
        (scheme-tools srfi-compat :43)
        (scheme-tools value-number)
        (scheme-tools)
        (sine coroutine-id)
        (sine coroutine-interpreter)
        (sine debug)
        (sine delimcc-simple-r6rs)
        (sine desugar)
        (sine preamble)
        (sine syntax)
        (sine utils))



;; --------------------------------------------------------------------
;; DP tables


;; Global marginals

(define global-marginals
  (make-eq-hashtable))

(define (set-global-marginal! &args dist)
  (hashtable-set! global-marginals &args dist))

(define (get-global-marginal &args)
  (hashtable-ref/default global-marginals
                         &args
                         (lambda ()
                           (let ([marginal (make-empty-dist)])
                             (set-global-marginal! &args marginal)
                             marginal))))

(define (increment-global-marginal! &args v p)
  (let ([marginal (get-global-marginal &args)])
    (let ([new-p (logsumexp (get-dist-prob marginal v) p)])
      (assert* (<= new-p (+ LOG-PROB-1 .000001))
               (lambda () (pen (exp new-p))))
      (set-dist-prob! marginal v new-p)
      (assert* (<= (dist-mass marginal) (+ LOG-PROB-1 .000001))
               (lambda () (pen (exp (dist-mass marginal)))))
      marginal)))


;; Local marginals

(define local-marginals
  (make-eq-hashtable))

(define (set-local-marginal! &args &slot fdist)
  (hashtable-set! global-marginals (&cons &args &slot) fdist))

(define (get-local-marginal &args &slot)
  (hashtable-ref/default local-marginals
                         (&cons &args &slot)
                         (lambda ()
                           (let ([marginal (make-empty-fdist)])
                             (set-local-marginal! &args &slot marginal)
                             marginal))))


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

(define (get-unexplored-mass parent-args &slot &slot-elts)
  (map (lambda (&slot-elt)
         (log1minus (get-explored-mass parent-args
                                       (&reverse (&cons &slot-elt (&reverse &slot))))))
       &slot-elts))

(define (update-explored-mass! &args &slot)
  (pen "update-explored-mass! " (args->string &args) " " (slot->string &slot))
  (when (not (fully-explored? &args &slot))
        (let*-values ([(marginal) (get-local-marginal &args &slot)]
                      [(vals ps) (dist-vals&ps marginal)])
          (when (not (= (vector-length vals) 0))
                (let ([weighted-child-mass
                       (apply logsumexp
                              (vector->list
                               (vector-map (lambda (&v p) (+ p (get-explored-mass &args (&reverse (&cons &v (&reverse &slot))))))
                                           vals ps)))])
                  (set-explored-mass! &args &slot weighted-child-mass)
                  (assert (not (= weighted-child-mass LOG-PROB-0)))))))
  (when (not (&expand-boolean (&null? &slot)))
        (update-explored-mass! &args (&reverse (&cdr (&reverse &slot))))))

(define (reweight-dist dist log-weights)
  (make-dist (dist-vals dist)
             (vector-map +
                         (dist-ps dist)
                         log-weights)))

(define (reweight-fdist fdist log-weights)
  (let ([fragments (fdist-fragments fdist)])
    (fragments->fdist (map (lambda (fragment log-weight)
                             (make-fragment (fragment-value fragment)
                                            (+ (fragment-prob fragment)
                                               log-weight)
                                            (fragment-id fragment)))
                           fragments
                           log-weights))))

(define (reweight-unexplored/dist &args &slot dist path-is-new)
  (if (or (eq? &args 'top)
          path-is-new)
      dist
      (let* ([unexplored-mass (get-unexplored-mass &args &slot (vector->list (dist-vals dist)))]
             [reweighted-dist (reweight-dist dist (list->vector unexplored-mass))])
        reweighted-dist)))

(define (reweight-unexplored/fdist &args &slot fdist path-is-new)
  (if (or (eq? &args 'top)
          path-is-new)
      fdist
      (let* ([fragment-&ids (map (compose compress-symbol fragment-id)                                                                ;; format of fragment ids unclear
                                 (fdist-fragments fdist))]
             [unexplored-mass (get-unexplored-mass &args &slot fragment-&ids)]
             [reweighted-fdist (reweight-fdist fdist unexplored-mass)])
        reweighted-fdist)))

(define (new-path-position? &args &slot)
  (let ([v (= (get-explored-mass &args &slot) LOG-PROB-0)])
    ;; (pen "new-path-position? " (args->string &args) " " (slot->string &slot) ": " v)
    v))



;; --------------------------------------------------------------------
;; Sampler
;;
;; Note: path and slot are in inverse order (last element first) until used


(define top-subcall
  (make-subcall 'top-cont 'top-call 'top))

(define (sample-top init-state)
  (let ([init-stack (list top-subcall)]
        [&init-slot &null]
        [init-score LOG-PROB-1]
        [&init-path &null]
        [path-is-new #f])
    (assert (subcall? init-state))
    (let-values ([(&value score _fid &path path-is-new*)
                  (sample init-state init-stack &init-slot init-score &init-path path-is-new)])
      (pen "path-is-new: " path-is-new*)
      (values (&expand-recursive &value)
              score
              (&reverse &path)))))

(define (sample state stack &slot score &path path-is-new)
  (cond [(terminal? state) (sample-terminal state stack &slot score &path path-is-new)]
        [(xrp? state) (sample-xrp state stack &slot score &path path-is-new)]
        [(subcall? state) (sample-subcall state stack &slot score &path path-is-new)]
        [else (error state "unknown state type")]))

(define (sample-terminal terminal stack &slot score &path path-is-new)
  (values (terminal-value terminal)
          score
          (terminal-id terminal)                                                                                               ;; this is equal to terminal-value
          (&cons (&list (compress-symbol 'terminal)) &path)
          path-is-new))

(define (sample-xrp xrp stack &slot score &path path-is-new)
  (pen "sampling xrp at " (args->string (subcall-args (car stack))) ", " (slot->string (&reverse &slot)))
  (let* ([xrp-dist (make-dist (xrp-vals xrp) (xrp-probs xrp))]
         [reweighted-dist (reweight-unexplored/dist (subcall-args (car stack)) (&reverse &slot) xrp-dist path-is-new)]         ;; here, we reweight a plain dist
         [value (sample-dist reweighted-dist)]
         [inc-score (get-dist-prob xrp-dist value)]
         [&slot* (&cons value &slot)]
         [path-is-new* (or path-is-new (new-path-position? (subcall-args (car stack)) (&reverse &slot*)))])
    (sample ((xrp-cont xrp) value)
            stack
            &slot*
            (+ score inc-score)
            (&cons (&list (compress-symbol 'xrp)
                          value
                          (compress-number inc-score)
                          (xrp-id xrp))
                   &path)
            path-is-new*)))

(define (sample-subcall subcall stack &slot score &path path-is-new)
  (let* ([global-marginal (get-global-marginal (subcall-args subcall))]
         [local-marginal (get-local-marginal (subcall-args (car stack)) (&reverse &slot))]
         [diff-marginal (diff-dist-fdist global-marginal local-marginal)]
         [reweighted-local-marginal (reweight-unexplored/fdist (subcall-args (car stack))                                       ;; here, we reweight a fragment dist
                                                               (&reverse &slot)
                                                               local-marginal
                                                               path-is-new)])
    (let ([dispatch (sample-discrete/log
                     (list (cons sample-subcall-old (fdist-mass reweighted-local-marginal))
                           (cons sample-subcall-new (dist-mass diff-marginal))
                           (cons sample-subcall-internal (safe-log1minus (dist-mass global-marginal)))))])
      (let-values ([(value inc-score &fragment-id &subpath path-is-new*)                                                        ;; &fragment-id must be a compressed object whenever it's returned
                    (dispatch subcall stack reweighted-local-marginal local-marginal diff-marginal)])
        (let* ([&path* (&cons (&list (compress-symbol 'subcall)
                                     (subcall-args subcall)
                                     value
                                     (compress-number inc-score)
                                     (&reverse &subpath))
                              &path)]
               [&slot* (&cons &fragment-id &slot)])
          (sample ((subcall-cont subcall) value)
                  stack
                  &slot*
                  (+ score inc-score)
                  &path*
                  (or path-is-new
                      path-is-new*
                      (new-path-position? (subcall-args (car stack)) (&reverse &slot*)))))))))

(define (fragment-id-placeholder)
  (&cons (compress-symbol 'fragment-id-placeholder)
         (compress-symbol (gensym))))

(define (unknown-fragment-id? &id)
  (and (&expand-boolean (&pair? &id))
       (&expand-boolean (&symbol? (&car &id)))
       (eq? (&expand-symbol (&car &id)) 'fragment-id-placeholder)))

(define (sample-subcall-old subcall stack reweighted-local-marginal local-marginal diff-marginal)
  (let ([fragment (sample-fdist-fragment reweighted-local-marginal)])
    (values (fragment-value fragment)
            (fragment-prob fragment)
            (fragment-id fragment)
            &null
            false)))

(define (sample-subcall-new subcall stack reweighted-local-marginal local-marginal diff-marginal)
  (pen "NEW")
  (let* ([value (sample-dist diff-marginal)]
         [inc-score (get-dist-prob diff-marginal value)])
    (values value inc-score (fragment-id-placeholder) &null true)))

(define (sample-subcall-internal subcall stack reweighted-local-marginal local-marginal diff-marginal)
  (pen "INTERNAL")
  (let ([next-state (reset (make-terminal (apply-subcall subcall)))])
    (sample next-state (cons subcall stack) &null LOG-PROB-1 &null #f)))


;; --------------------------------------------------------------------
;; Updating

(define (hashtable-cons! table key elt)
  (hashtable-set! table
                  key
                  (cons elt (hashtable-ref table key '()))))


(define (make-set-hashtable! table)
  (let ([table* (make-eq-hashtable)])
    (hashtable-for-each (lambda (k lst)
                          (hashtable-set! table* k (delete-duplicates lst equal?)))
                        table)
    table*))


;; could delete duplicates in slot-sequences
(define (parse-path &path &top-value top-score)

  (define subcall-paths (make-eq-hashtable))
  (define global-to-local (make-eq-hashtable))
  (define slot-sequences '())
  (define terminals '())
  (define xrp-ids '())

  (define (parse-terminal! &path p-&args &slot)
    (set! terminals (cons (pair p-&args (&reverse &slot)) terminals))
    (assert* (&expand-boolean (&null? (&cdr &path))) (lambda () (pen (&expand-recursive &path))))
    (parse! (&cdr &path) p-&args &slot)) ;;;; <----- ????

  (define (parse-subcall! &path p-&args &slot)
    (let-values ([(state-type args value score subpath) (apply values (&expand-list (&car &path)))])
      (hashtable-cons! global-to-local args (cons p-&args (&reverse &slot)))
      (when (not (&expand-boolean (&null? subpath)))
            (hashtable-cons! subcall-paths args (list subpath value score))
            (parse! subpath args &null))
      (parse! (&cdr &path) p-&args (&cons value &slot))))

  (define (parse-xrp! &path p-&args &slot)
    (let-values ([(state-type value score id) (apply values (&expand-list (&car &path)))])
      (set! xrp-ids (cons id xrp-ids))
      (hashtable-cons! global-to-local id (cons p-&args (&reverse &slot)))
      ;; (hashtable-cons! subcall-paths id (list &null value score))
      (parse! (&cdr &path) p-&args (&cons value &slot))))

  (define (parse! &path p-&args &slot)
    (if (&expand-boolean (&null? &path))
        (set! slot-sequences
              (cons (cons p-&args (&reverse &slot))
                    slot-sequences))
        (let* ([step (&car &path)]
               [state-type (&expand-symbol (&car step))])
          (cond [(eq? state-type 'terminal) (parse-terminal! &path p-&args &slot)]
                [(eq? state-type 'subcall) (parse-subcall! &path p-&args &slot)]
                [(eq? state-type 'xrp) (parse-xrp! &path p-&args &slot)]))))

  (parse! &path 'top &null)
  ;; (hashtable-cons! subcall-paths 'top (list &path &top-value (compress-number top-score)))
  (set! subcall-paths (make-set-hashtable! subcall-paths))
  (set! global-to-local (make-set-hashtable! global-to-local))
  (values subcall-paths terminals xrp-ids slot-sequences global-to-local))


(define (update-global-marginals! subcall-paths)
  (hashtable-for-each (lambda (args lst)
                        (map (lambda (elt)
                               (let-values ([(&subpath &value &score) (apply values elt)])
                                 (pen "increment-global-marginal! " (args->string args) " "
                                      (&->string:n &value 30) " " (number->string (exp (&expand-number &score))))
                                 (increment-global-marginal! args &value (&expand-number &score))))
                             lst))
                      subcall-paths))

(define (update-local-marginals! global-to-local) ;; take into account chunks?
  (hashtable-for-each (lambda (args lst)
                        (alist-map (lambda (parent-args slot)
                                     (set-local-marginal! parent-args
                                                          slot
                                                          (copy-dist (get-global-marginal args))))
                                   lst))
                      global-to-local))

(define (update-terminals-explored-mass! terminals)
  (alist-map (lambda (args slot)
               (set-explored-mass! args slot LOG-PROB-1))
             terminals))

(define (update-seqs-explored-mass! slot-sequences)
  (alist-map (lambda (args slot)
               ;; (pen "\nupdate-seqs-explored-mass! " args " " (&expand-recursive slot))
               (set-explored-mass! args slot LOG-PROB-1)
               (update-explored-mass! args slot))
             slot-sequences))

(define (update-xrp-global-marginals! xrp-ids)
  (for-each (lambda (id)
              (set-global-marginal! id
                                    (make-dist (xrp-id->xrp-vals id)
                                               (xrp-id->xrp-probs id))))
            xrp-ids))

(define (args->string args)
  (if (eq? args 'top)
      'top
      (string-append (->string args) " " (maybe-syntax->string args))))

(define (update! path top-value top-score)
  (let-values ([(subcall-paths terminals xrp-ids slot-sequences global-to-local)
                (parse-path path top-value top-score)])
    (pen "\nXRP ids:\n" xrp-ids)
    (pen "\nTerminals:")
    (alist-map (lambda (args slot)
                 (pen (args->string args) ", " (slot->string slot) ", p=1"))
               terminals)
    (pen "\nSlot sequences:")
    (alist-map (lambda (k v) (pen (args->string k) ", " (&expand-recursive v)))
               slot-sequences)
    (pen "\nSubcall paths:")
    (hashtable-for-each (lambda (args lst)
                          (pen (args->string args) ": ")
                          (for-each (lambda (sp) (pen "  "(first sp) ", "
                                                 (&expand-recursive (second sp)) ", "
                                                 (exp (&expand-recursive (third sp))))) lst))
                        subcall-paths)
    (pen "\nGlobal-to-local:")
    (hashtable-for-each (lambda (args lst)
                          (alist-map (lambda (parent-args slot)
                                       (pen (args->string parent-args) ", " (slot->string slot) " == " (args->string args)))
                                     lst))
                        global-to-local)
    (update-xrp-global-marginals! xrp-ids)
    (update-global-marginals! subcall-paths)
    (update-local-marginals! global-to-local)
    (update-terminals-explored-mass! terminals)
    (update-seqs-explored-mass! slot-sequences)
    (pen "\nGlobal marginals:")
    (hashtable-for-each (lambda (args dist)
                          (pen (args->string args) ": " (dist->string dist)))
                        global-marginals)
    (pen "\nLocal marginals:")
    (hashtable-for-each (lambda (id dist)
                          (let ([arg+slot (&expand-pair id)])
                            (pen (args->string (car arg+slot)) ", slot " (&expand-recursive (cdr arg+slot))
                                 ": " (dist->string dist))))
                        local-marginals)
    (pen "\nExplored mass:")
    (hashtable-for-each (lambda (k v)
                          (let ([args+slot (&expand-pair k)])
                            (pen (args->string (car args+slot)) ", " (&expand-recursive (cdr args+slot)) ": " (exp v))))
                        explored-mass-table)))


;; --------------------------------------------------------------------
;; Test

(define example:two-flips
  '(list (flip .7) (flip .4)))

(define example:multiple-paths-or
  '(list (or (flip .5) (flip .6))
         (flip .7)))

(define example:multiple-paths-basic
  '(list (if (flip .4) 1 1)
         (flip .3)))

(define example:no-multiple-paths
  '(list (flip .1) (list (list (flip .2) (flip .1)) (list (flip .2) (flip .1)))))

(define (test)
  (let ([expr example:multiple-paths-basic])
    (pen "--------------------------------------------------------------------")
    (let-values ([(value score path) (sample-top (coroutine-interpreter expr))])
      (pen "value: " value)
      (pen "score: " (exp score))
      ;; (update! path value score)
      value)))

(seed-rng 10)
(repeat 30 test)