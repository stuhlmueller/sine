#!r6rs

;; - convert equations to graph
;; - cluster into sccs

(library

 (sine spn-solve)

 (export solve-equations)

 (import (rnrs)
         (sine hashtable)
         (sine spn-equations)
         (scheme-tools watcher)
         (scheme-tools math iterate)
         (scheme-tools graph scsh-components)
         (scheme-tools srfi-compat :1)
         (scheme-tools))

 (define (special? sym)
   (contains? '(= + - * / log exp logsumexp if or LOG-PROB-1 LOG-PROB-0) sym eq?))

 (define (equation->symbols equation eqn-filter)
   (cond [(null? equation) '()]
         [(special? equation) '()]
         [(eqn-filter equation) '()]
         [(symbol? equation) (list equation)]
         [(list? equation) (apply append (map (lambda (eqn) (equation->symbols eqn eqn-filter))
                                              equation))]
         [else '()]))

 (define (equations->symbols equations eqn-filter)
   (unique
    (fold (lambda (equation symbols)
            (append (equation->symbols equation eqn-filter) symbols))
          '()
          equations)))

 (define (extract-ids eqn)
   (cond [(list? eqn) (apply append (map extract-ids eqn))]
         [(and (symbol? eqn) (not (special? eqn))) (list eqn)]
         [else '()]))

 (define (equations->scsh-graph eqns)
   (let* ([eqn-table (make-eq-hashtable)]
          [scsh-graph
           (map (lambda (eqn)
                  (hashtable-set! eqn-table (second eqn) eqn)
                  (pair (second eqn)
                        (extract-ids (third eqn))))
                eqns)])
     (values scsh-graph eqn-table)))

 (define (iterate-with-message equations)
   (let ([iterator-env (environment '(rnrs) '(scheme-tools math))])
     (let-values ([(solutions final-delta) (iterate/eqns equations 0.0 'env iterator-env)])
       (when (not (= final-delta 0.0))
             (pe "fixed-point iterator: final delta " final-delta "\n"))
       solutions)))

 (define (unique objects)
   (let ([seen? (make-watcher)])
     (filter (lambda (obj) (not (seen? obj))) objects)))

 (define (relevant-solution-equations equations solutions)
   (let* ([symbols (equations->symbols equations (lambda (x) #f))]
          [equations (filter-map (lambda (symbol)
                                   (let ([binding (hashtable-ref solutions symbol #f)])
                                     (if binding
                                         `(= ,symbol ,binding)
                                         #f)))
                                 symbols)])
     (unique equations)))

 (define (get-component-equations eqn-table component solutions)
   (let* ([equations-1 (map (lambda (id) (hashtable-ref/nodef eqn-table id))
                            component)]
          [equations-2 (relevant-solution-equations equations-1 solutions)])
     (append equations-1
             equations-2)))

 (define (solve-equations eqns)
   (let-values ([(scsh-graph eqn-table) (equations->scsh-graph eqns)])
     (let ([components (reverse (scsh-strongly-connected-components scsh-graph))]
           [solutions (make-eq-hashtable)])
       (for-each (lambda (component)
                   (let* ([component-equations (simplify-equations (get-component-equations eqn-table component solutions))]
                          [new-solutions (iterate-with-message component-equations)])
                     (for-each (lambda (binding)
                                 (hashtable-set!/assert-consistent solutions
                                                                   (first binding)
                                                                   (rest binding)))
                               new-solutions)))
                 components)
       solutions)))

 )
