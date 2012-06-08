#!r6rs

(library

 (sine spn)

 (export marginalize
         lookup-marginals)

 (import (rnrs)
         (scheme-tools)
         (scheme-tools macros)
         (scheme-tools implementation-specific)
         (sine settings)
         (sine build-spn)
         (sine preamble)
         (sine spn-solve)
         (sine hashtable)
         (sine spn-equations)
         (sine coroutine-interpreter)
         (sine value-number)
         (sine coroutine-id))

 (define (verbose-pe . args)
   (when verbose
         (apply pe args)))

 (define (lookup-marginals solutions root-terminal-vars root-terminal-vals)
   (string-sort
    (map (lambda (terminal-var terminal-val)
           (pair terminal-val
                 (hashtable-ref/nodef solutions terminal-var)))
         root-terminal-vars
         root-terminal-vals)))

 (define/kw (marginalize expr [max-spn-size :default +inf.0])
   (let* ([interpreter-thunk (lambda () (coroutine-interpreter (with-preamble expr)))]
          [_ (verbose-pe "Building SPN...\n")]
          [spn (opt-timeit verbose (build-spn interpreter-thunk 'max-spn-size max-spn-size))]
          [_ (verbose-pe "SPN # edges: " (hashtable-size (spn->edges spn)) "\n")]
          [_ (verbose-pe "Building equations...\n")])
     (let-values ([(equations root-terminal-vars) (spn-equations spn)])
       (let* ([_ (verbose-pe "Solving equations...\n")]
              [solutions (opt-timeit verbose (solve-equations equations))]
              [_ (verbose-pe "done!\n")]
              [root-terminal-vals (map (lambda (terminal-id) (&expand-recursive (terminal-id->value terminal-id)))
                                       (hashtable-ref (spn->terminal-ids spn) 'root '()))])
         (opt-timeit verbose (lookup-marginals solutions root-terminal-vars root-terminal-vals))))))

 )








