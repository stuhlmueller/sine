#!r6rs

(import (rnrs)
        (scheme-tools)
        (scheme-tools object-id)
        (scheme-tools macros)
        (scheme-tools implementation-specific)
        (sine debug)
        (sine utils)
        (sine spn)
        (sine settings)
        (sine build-spn)
        (sine preamble)
        (sine spn-solve)
        (sine hashtable)
        (sine spn-equations)
        (sine coroutine-interpreter)
        (sine value-number)
        (sine coroutine-id))

(define counter
  (get-counter))

(define (terminal-info spn node-ids terminal-id)
  (&->string:n (terminal-id->value terminal-id) 30))

(define (indicator-info spn node-ids node)
  (string-append "Ind "
                 (terminal-info spn
                                node-ids
                                (hashtable-ref (spn->indicator-ids spn)
                                               node
                                               #f))))

(define (ref-info spn node-ids node)
  (let ([subroot-id (hashtable-ref (spn->ref-subroot-ids spn)
                                   node
                                   #f)])
    (string-append
     "Ref "
     (subroot-info spn node-ids subroot-id)
     " => "
     (terminal-info spn
                    node-ids
                    (hashtable-ref (spn->ref-terminal-ids spn)
                                   node
                                   #f)))))

(define (subroot-number node-ids node)
  (number->string (hashtable-ref node-ids node #f)))

(define (subroot-info spn node-ids node)
  (if (eq? node 'root)
      'root
      (string-append (subroot-number node-ids node)
                     " "
                     (recur-state->string node ;; (id->object (sym+num->num node))
                                          'num-chars 10
                                          'show-env #t))))

(define (prob-info spn node-ids node)
  (hashtable-ref (spn->probs spn) node #f))

(define (node-info spn node-ids node)
  (let ([node-type (hashtable-ref (spn->nodetypes spn) node #f)])
    (cond [(eq? node-type 'indicator) (indicator-info spn node-ids node)]
          [(eq? node-type 'ref) (ref-info spn node-ids node)]
          [(eq? node-type 'root) (subroot-info spn node-ids node)]
          [(eq? node-type 'sum) "+"]
          [(eq? node-type 'product) "X"]
          [(eq? node-type 'prob) (prob-info spn node-ids node)]
          [else ""])))

(define (make-tgf expr)
  (let* ([interpreter-thunk (lambda () (coroutine-interpreter (with-preamble expr)))]
         [spn (build-spn interpreter-thunk)]
         [equations (spn-equations spn)]
         [solutions (opt-timeit verbose (solve-equations equations))]
         [marginals (lookup-marginals spn solutions)])
    ;; (pe (log-marginal->marginal marginals))
    (let ([nodes (make-eq-hashtable)])
      (hashtable-for-each (lambda (source targets)
                            (hashtable-set! nodes source (counter))
                            (for-each (lambda (target)
                                        (hashtable-set! nodes target (counter)))
                                      targets))
                          (spn->edges spn))
      (hashtable-for-each (lambda (node i)
                            (pe i " " (node-info spn nodes node) "\n"))
                          nodes)
      (pe "#\n")
      (hashtable-for-each (lambda (source targets)
                            (for-each (lambda (target)
                                        (pe (hashtable-ref nodes source #f) " "
                                            (hashtable-ref nodes target #f) "\n"))
                                      targets))
                          (spn->edges spn))
      )))

(define expr
  '(query/cache
    (define x (flip))
    (define y (flip))
    x
    (or x y)))

(define game-expr
  '(begin
     (define game
       (lambda (player)
         (cache
          (if (flip .6)
              (not (game (not player)))
              (if player
                  (flip .2) (flip .7))))))
     (game true)))

(make-tgf game-expr)
