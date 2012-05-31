#!r6rs

(library

 (sine spn-equations)

 (export spn-equations
         simplify-equations)

 (import (rnrs)
         (sine settings)
         (sine build-spn)
         (sine hashtable)
         (sine value-number)
         (scheme-tools macros)
         (scheme-tools srfi-compat :1)
         (scheme-tools watcher)
         (scheme-tools))

 (define seen? (make-watcher))

 (define (get-root-ids spn)
   (let-values ([(keys vals) (hashtable-entries (spn->nodetypes spn))])
     (let ([root-ids '()])
       (vector-for-each (lambda (id type)
                          ;; (pe id " " type "\n")
                          (when (eq? type 'root)
                                (set! root-ids (cons id root-ids))))
                        keys
                        vals)
       root-ids)))

 (define (spn-equations spn)
   (let ([eqns (opt-timeit verbose (build-all-equations spn))])
     (when verbose (pe "Equations: " (length eqns) "\n"))
     (let ([simplified-eqns (opt-timeit verbose (simplify-equations eqns))])
       simplified-eqns)))

 (define (build-all-equations spn)
   (let* ([root-ids (get-root-ids spn)]
          [eqns '()]
          [add-eqn! (lambda (eqn) (set! eqns (cons eqn eqns)))])
     (for-each (lambda (root-id)
                 (let ([terminal-ids (hashtable-ref/nodef (spn->terminal-ids spn) root-id)])
                   (for-each (lambda (terminal-id)
                               (build-equations spn terminal-id root-id add-eqn!))
                             terminal-ids)))
               root-ids)
     eqns))

 (define (build-equations spn terminal-id root-id add-eqn!)
   (if (seen? (list 'build-equations terminal-id root-id))
       '()
       (let ([tid (lambda (id) (sym-append id terminal-id))])
         (let build ([from-id root-id])
           (if (seen? (list 'build terminal-id root-id from-id))
               #f
               (let ([from-type (hashtable-ref/nodef (spn->nodetypes spn) from-id)])
                 (cond [(eq? from-type 'sum)
                        (let ([child-ids (hashtable-ref/nodef (spn->edges spn) from-id)])
                          (add-eqn! (if (= (length child-ids) 1)
                                        `(= ,(tid from-id) ,@(map tid child-ids))
                                        `(= ,(tid from-id) (logsumexp ,@(map tid child-ids)))))
                          (for-each build child-ids))]
                       [(eq? from-type 'root)
                        (let ([child-ids (hashtable-ref/nodef (spn->edges spn) from-id)])
                          (assert (= (length child-ids) 1))
                          (add-eqn! `(= ,(tid from-id) ,@(map tid child-ids)))
                          (for-each build child-ids))]
                       [(eq? from-type 'product)
                        (let ([child-ids (hashtable-ref/nodef (spn->edges spn) from-id)])
                          (add-eqn! (if (= (length child-ids) 1)
                                        `(= ,(tid from-id) ,@(map tid child-ids))
                                        `(= ,(tid from-id) (+ ,@(map tid child-ids)))))
                          (for-each build child-ids))]
                       [(eq? from-type 'indicator)
                        (let ([ind-id (hashtable-ref/nodef (spn->indicator-ids spn) from-id)])
                          (add-eqn! (if (eq? ind-id terminal-id)
                                        `(= ,(tid from-id) LOG-PROB-1)
                                        `(= ,(tid from-id) LOG-PROB-0))))]
                       [(eq? from-type 'ref)
                        (let ([subroot-id (hashtable-ref/nodef (spn->ref-subroot-ids spn) from-id)]
                              [term-id (hashtable-ref/nodef (spn->ref-terminal-ids spn) from-id)])
                          (add-eqn! `(= ,(tid from-id) ,(sym-append subroot-id term-id)))
                          (build-equations spn term-id subroot-id add-eqn!))]
                       [(eq? from-type 'prob)
                        (add-eqn! `(= ,(tid from-id) ,(hashtable-ref/nodef (spn->probs spn) from-id)))]
                       [else (error from-type "unknown node type")])))))))

 (define (update-replacements! replacements eqns)
   (let ([new-eqns '()])
     (for-each (lambda (eqn)
                 (if (and (= (length eqn) 3)
                          (eq? (first eqn) '=)
                          (symbol? (second eqn))
                          (not (prefixed-symbol? (second eqn) 'root))
                          (not (list? (third eqn))))
                     (hashtable-set! replacements
                                     (second eqn)
                                     (third eqn))
                     (set! new-eqns (cons eqn new-eqns))))
               eqns)
     (values replacements new-eqns)))

 (define (simplify-equations eqns)
   (let ([replacements (make-eq-hashtable)])
     (let outer-loop ([eqns-1 eqns]
                      [outer-change #f])
       (let-values ([(replacements eqns-2) (update-replacements! replacements eqns-1)])
         (let inner-loop ([eqns-3 eqns-2])
           (let-values ([(eqns-4 inner-change) (replace-all eqns-3 replacements)])
             (when inner-change
                   (set! outer-change #t))
             (if inner-change
                 (inner-loop eqns-4)
                 (if outer-change
                     (outer-loop eqns-4 #f)
                     eqns-4))))))))

 (define (replace-all eqns replacements)
   (let ([changed #f])
     (define (%replace-all eqn)
       (cond

        [(and (list? eqn)
              (> (length eqn) 1)
              (eq? (first eqn) 'logsumexp)
              (any (lambda (x) (eq? x 'LOG-PROB-0)) eqn))
         (let ([remainder (filter (lambda (x) (not (eq? x 'LOG-PROB-0)))
                                  (cdr eqn))])
           (set! changed #t)
           (if (null? remainder)
               'LOG-PROB-0
               `(logsumexp ,@remainder)))]

        [(and (list? eqn)
              (> (length eqn) 1)
              (eq? (first eqn) '+)
              (any (lambda (x) (eq? x 'LOG-PROB-1)) eqn))
         (let ([remainder (filter (lambda (x) (not (eq? x 'LOG-PROB-1)))
                                  (cdr eqn))])
           (set! changed #t)
           (if (null? remainder)
               'LOG-PROB-0
               `(+ ,@remainder)))]

        [(and (list? eqn)
              (> (length eqn) 1)
              (eq? (first eqn) '+)
              (any (lambda (x) (eq? x 'LOG-PROB-0)) eqn))
         (begin
           (set! changed #t)
           'LOG-PROB-0)]

        [(and (list? eqn)
              (= (length eqn) 2)
              (or (eq? (first eqn) '+)
                  (eq? (first eqn) 'logsumexp)))
         (begin
           (set! changed #t)
           (second eqn))]

        [(list? eqn) (map (lambda (elt) (%replace-all elt)) eqn)]
        [(symbol? eqn) (let ([v (hashtable-ref replacements eqn eqn)])
                         (when (not (equal? v eqn))
                               (set! changed #t))
                         v)]
        [else eqn]))
     (let ([new-eqns (map (lambda (eqn)
                            `(= ,(second eqn)
                                ,@(%replace-all (cddr eqn))))
                          eqns)])
       (values new-eqns
               changed))))

 )
