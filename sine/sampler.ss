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
;; Settings

(define verbose #f)



;; --------------------------------------------------------------------
;; Fragment IDs

(define make-fragment-id (get-counter))

(define (fragment-id-placeholder)
  (&cons (compress-symbol 'fid-placeholder)
         (compress-number (make-fragment-id))))

(define (fragment-id-placeholder? &id)
  (and (&pair?->b &id)
       (&symbol?->b (&car &id))
       (eq? (&expand-symbol (&car &id)) 'fid-placeholder)))



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
    (let ([new-p (safe-logsumexp (get-dist-prob marginal v) p)])
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
  (hashtable-set! local-marginals (&cons &args &slot) fdist))

(define (get-local-marginal &args &slot)
  (hashtable-ref/default local-marginals
                         (&cons &args &slot)
                         make-empty-fdist))

(define (get/make-local-marginal &args &slot)
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

(define (set-explored-mass! &args &slot mass)
  (hashtable-set! explored-mass-table
                  (&cons &args &slot)
                  mass))

(define (inc-explored-mass! &args &slot inc-mass)
  (when verbose
        (pen "inc-explored-mass! " (args->string &args) ", "
             (slot->string &slot) " inc-mass: " inc-mass))
  (let* ([old-mass (get-explored-mass &args &slot)]
         [new-mass (safe-logsumexp old-mass inc-mass)])
    (assert* (<= new-mass LOG-PROB-1)
             (lambda () (pen "inc-explored-mass error: logprob > 0\n"
                        "old-mass: " old-mass "\n"
                        "new mass: " new-mass)))
    (set-explored-mass! &args
                        &slot
                        new-mass)))

(define (get-unexplored-mass parent-args &slot &slot-elts)
  (map (lambda (&slot-elt)
         (log1minus (get-explored-mass parent-args
                                       (&reverse (&cons &slot-elt (&reverse &slot))))))
       &slot-elts))

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
      (let* ([fragment-&ids (map fragment-id (fdist-fragments fdist))]
             [unexplored-mass (get-unexplored-mass &args &slot fragment-&ids)]
             [reweighted-fdist (reweight-fdist fdist unexplored-mass)])
        reweighted-fdist)))

(define (new-path-position? &args &slot)
  (let ([v (= (get-explored-mass &args &slot) LOG-PROB-0)])
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
          (terminal-id terminal)
          (&cons (&list (compress-symbol 'terminal)) &path)
          path-is-new))

(define (sample-xrp xrp stack &slot score &path path-is-new)
  (when verbose
        (pen "sampling xrp at " (args->string (subcall-args (car stack))) ", "
             (slot->string (&reverse &slot))))
  (let* ([xrp-dist (make-dist (xrp-vals xrp) (xrp-probs xrp))]
         [reweighted-dist (reweight-unexplored/dist (subcall-args (car stack))
                                                    (&reverse &slot) xrp-dist path-is-new)]
         [value (sample-dist reweighted-dist)]
         [inc-score (get-dist-prob xrp-dist value)]
         [&slot* (&cons value &slot)]
         [path-is-new* (or path-is-new
                           (new-path-position? (subcall-args (car stack)) (&reverse &slot*)))])
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
         [reweighted-local-marginal (reweight-unexplored/fdist (subcall-args (car stack))
                                                               (&reverse &slot)
                                                               local-marginal
                                                               path-is-new)])
    (when verbose
          (pen "\nslot:    " (slot->string &slot) "\n"
               "subcall: " (args->string (subcall-args subcall)) ":\n"
               "local: " (fdist->string local-marginal) " -- " (exp (fdist-mass local-marginal)) "\n"
               "reweighted: " (fdist->string reweighted-local-marginal)
               " -- " (exp (fdist-mass reweighted-local-marginal)) "\n"
               "diff: " (dist->string diff-marginal) " -- " (exp (dist-mass diff-marginal)) "\n"
               "global: " (dist->string global-marginal) " -- " (exp (dist-mass global-marginal))))
    (let ([dispatch (sample-discrete/log
                     (list (cons sample-subcall-old (fdist-mass reweighted-local-marginal))
                           (cons sample-subcall-new (dist-mass diff-marginal))
                           (cons sample-subcall-internal (safe-log1minus (dist-mass global-marginal)))))])
      (let-values ([(value inc-score &fragment-id &subpath path-is-new*)
                    (dispatch subcall stack reweighted-local-marginal local-marginal diff-marginal)])
        (let* ([&path* (&cons (&list (compress-symbol 'subcall)
                                     (subcall-args subcall)
                                     value
                                     (compress-number inc-score)
                                     (&reverse &subpath)
                                     (if (fragment-id-placeholder? &fragment-id)
                                         (compress-symbol 'unknown-fragment-id)
                                         &fragment-id))
                              &path)]
               [&slot* (&cons &fragment-id &slot)])
          (when verbose
                (pen (args->string (subcall-args subcall)) " -> "
                     (&->string:n value 30) ", " (exp inc-score)))
          (sample ((subcall-cont subcall) value)
                  stack
                  &slot*
                  (+ score inc-score)
                  &path*
                  (or path-is-new
                      path-is-new*
                      (new-path-position? (subcall-args (car stack)) (&reverse &slot*)))))))))

(define (sample-subcall-old subcall stack reweighted-local-marginal local-marginal diff-marginal)
  (when verbose (pen "old"))
  (let* ([fragment-logprobs (map fragment-prob (fdist-fragments local-marginal))]
         [reweighted-fragment-probs (map (compose exp fragment-prob)
                                         (fdist-fragments reweighted-local-marginal))]
         [index (sample-discrete reweighted-fragment-probs)]
         [fragment (list-ref (fdist-fragments reweighted-local-marginal) index)]
         [logprob (list-ref fragment-logprobs index)])
    (values (fragment-value fragment)
            logprob
            (fragment-id fragment)
            &null
            false)))

(define (sample-subcall-new subcall stack reweighted-local-marginal local-marginal diff-marginal)
  (when verbose (pen "diff"))
  (let* ([value (sample-dist diff-marginal)]
         [inc-score (get-dist-prob diff-marginal value)])
    (values value inc-score (fragment-id-placeholder) &null true)))

(define (sample-subcall-internal subcall stack reweighted-local-marginal local-marginal diff-marginal)
  (when verbose (pen "internal"))
  (let ([next-state (reset (make-terminal (apply-subcall subcall)))])
    (let-values ([(&value score _fid &path path-is-new*)
                  (sample next-state (cons subcall stack) &null LOG-PROB-1 &null #f)])
      (values &value score (fragment-id-placeholder) &path path-is-new*))))



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


(define (parse-path &path &top-value top-score)

  (define subcall-paths (make-eq-hashtable))
  (define slot-sequences '())
  (define terminals '())
  (define xrp-ids '())
  (define fragment-ids (make-eq-hashtable)) ;; associates a path element with a fragment id
  (define fragment-info '()) ;; each entry is a tuple (args, slot, p, v, fragment-id)

  (define (parse-terminal! &path p-&args &slot &slot-ps)
    (set! terminals (cons (pair p-&args (&reverse &slot)) terminals))
    (assert* (&null?->b (&cdr &path)) (lambda () (pen (&expand-recursive &path))))
    (parse! (&cdr &path) p-&args &slot &slot-ps))

  (define (parse-subcall! &path p-&args &slot &slot-ps)
    (let-values ([(state-type args value score subpath &fragment-id)
                  (apply values (&expand-list (&car &path)))])
      (when (not (&null?->b subpath))
            (hashtable-cons! subcall-paths args (list subpath value score))
            (parse! subpath args &null &null))
      (let ([&fid (if (eq? (&expand-step &fragment-id) 'unknown-fragment-id)
                      (let ([id (hashtable-ref/default
                                 fragment-ids
                                 (&car &path)
                                 (lambda ()
                                   (let ([id (compress-number (make-fragment-id))])
                                     (hashtable-set! fragment-ids (&car &path) id)
                                     id)))])
                        (set! fragment-info
                              (cons (list p-&args &slot score value id)
                                    fragment-info))
                        id)
                      &fragment-id)])
        (parse! (&cdr &path) p-&args (&cons &fid &slot) (&cons score &slot-ps)))))

  (define (parse-xrp! &path p-&args &slot &slot-ps)
    (let-values ([(state-type value score id) (apply values (&expand-list (&car &path)))])
      (set! xrp-ids (cons id xrp-ids))
      (parse! (&cdr &path) p-&args (&cons value &slot) (&cons score &slot-ps))))

  (define (parse! &path p-&args &slot &slot-ps)
    (if (&null?->b &path)
        (when (not (eq? p-&args 'top))
              (set! slot-sequences
                    (cons (list p-&args (&reverse &slot) (&reverse &slot-ps))
                          slot-sequences)))
        (let* ([step (&car &path)]
               [state-type (&expand-symbol (&car step))])
          (cond [(eq? state-type 'terminal) (parse-terminal! &path p-&args &slot &slot-ps)]
                [(eq? state-type 'subcall) (parse-subcall! &path p-&args &slot &slot-ps)]
                [(eq? state-type 'xrp) (parse-xrp! &path p-&args &slot &slot-ps)]))))

  (parse! &path 'top &null &null)
  (set! subcall-paths (make-set-hashtable! subcall-paths))
  (set! slot-sequences (delete-duplicates slot-sequences equal?))
  (values subcall-paths terminals xrp-ids slot-sequences fragment-info))


(define (update-global-marginals! subcall-paths)
  (hashtable-for-each
   (lambda (args lst)
     (map (lambda (elt)
            (let-values ([(&subpath &value &score) (apply values elt)])
              (when verbose
                    (pen "increment-global-marginal! " (args->string args) " "
                         (&->string:n &value 30) " " (number->string (exp (&expand-number &score)))))
              (increment-global-marginal! args &value (&expand-number &score))))
          lst))
   subcall-paths))

(define (update-local-marginals! fragment-info)
  (for-each (lambda (entry)
              (let-values ([(&args &slot score value &fragment-id) (apply values entry)])
                (let ([local-marginal (get/make-local-marginal &args &slot)])
                  (fdist-add-fragment! local-marginal
                                       (make-fragment value (&expand-number score) &fragment-id)))))
            fragment-info))

(define (update-seqs-explored-mass! slot-sequences)
  (map (lambda (entry)
         (let-values ([(args &slot &scores) (apply values entry)])
           (let loop ([&slot (&reverse &slot)]
                      [&scores (&reverse &scores)]
                      [acc LOG-PROB-1])
             (inc-explored-mass! args (&reverse &slot) acc)
             (when (not (&null?->b &slot))
                   (loop (&cdr &slot)
                         (&cdr &scores)
                         (+ acc (&expand-number (&car &scores))))))))
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
  (let-values ([(subcall-paths terminals xrp-ids slot-sequences fragment-info)
                (parse-path path top-value top-score)])
    (when verbose
          (pen "\nXRP ids:\n" xrp-ids)
          (pen "\nTerminals:")
          (alist-map (lambda (args slot)
                       (pen (args->string args) ", " (slot->string slot) ", p=1"))
                     terminals)
          (pen "\nSlot sequences:")
          (alist-map (lambda (k v) (pen (args->string k) ", " (map &expand-recursive v)))
                     slot-sequences)
          (pen "\nSubcall paths:")
          (hashtable-for-each (lambda (args lst)
                                (pen (args->string args) ": ")
                                (for-each (lambda (sp) (pen "  "(first sp) ", "
                                                       (&expand-recursive (second sp)) ", "
                                                       (exp (&expand-recursive (third sp))))) lst))
                              subcall-paths)
          (pen "\nFragment info:")
          (for-each (lambda (entry)
                      (let-values ([(&args &slot score value &fragment-id) (apply values entry)])
                        (pen (&expand-number score) " " (&->string:n value 30) " "
                             (&->string:n &fragment-id 30))))
                    fragment-info)
          (pen))

    (update-xrp-global-marginals! xrp-ids)
    (update-global-marginals! subcall-paths)
    (update-local-marginals! fragment-info)
    (update-seqs-explored-mass! slot-sequences)

    (when verbose
          (pen "\nGlobal marginals:")
          (hashtable-for-each (lambda (args dist)
                                (pen (args->string args) ": " (dist->string dist)))
                              global-marginals)
          (pen "\nLocal marginals:")
          (hashtable-for-each (lambda (id dist)
                                (let ([arg+slot (&expand-pair id)])
                                  (pen (args->string (car arg+slot)) ", slot "
                                       (&expand-recursive (cdr arg+slot))
                                       ": " (fdist->string dist))))
                              local-marginals)
          (pen "\nExplored mass:")
          (hashtable-for-each (lambda (k v)
                                (let ([args+slot (&expand-pair k)])
                                  (pen (args->string (car args+slot)) ", "
                                       (&expand-recursive (cdr args+slot)) ": " (exp v))))
                              explored-mass-table))
    ))



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

(define example:function
  '(begin
     (define (foo x)
       (if x (flip .4) (flip .5)))
     (foo (flip .3))))

(define example:recursion
  '(begin
     (define foo
       (lambda ()
         (if (flip)
             (not (foo))
             (flip .3))))
     (cache (foo))))

(define (test)
  (let ([expr example:multiple-paths-or])
    (when verbose
          (pen "--------------------------------------------------------------------"
               "\nSAMPLING"))
    (let-values ([(value score path) (sample-top (coroutine-interpreter expr))])
      (pen "value: " value)
      (pen "score: " (exp score) "\n")
      (when verbose
            (pen "\n--------------------------------------------------------------------"
                 "\nUPDATING"))
      (update! path value score)
      value)))

(seed-rng 10)
(repeat 10 test)

(pen "Global marginals:")
(hashtable-for-each (lambda (args dist)
                      (pen (args->string args) ": " (dist->string dist)))
                    global-marginals)