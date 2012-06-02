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

 (define (lookup-marginals spn solutions)
   (let* ([terminal-ids (hashtable-ref (spn->terminal-ids spn) 'root '())]
          [eqn-ids (map (lambda (terminal-id) (sym-append 'root terminal-id))
                        terminal-ids)])
     (string-sort
      (map (lambda (eqn-id terminal-id)
             (let ([logprob (hashtable-ref/nodef solutions eqn-id)]
                   [value (&expand-recursive (terminal-id->value terminal-id))])
               (pair value logprob)))
           eqn-ids
           terminal-ids))))

 (define/kw (marginalize expr [max-spn-size :default +inf.0])
   (let* ([interpreter-thunk (lambda () (coroutine-interpreter (with-preamble expr)))]
          [_ (verbose-pe "Building SPN...\n")]
          [spn (opt-timeit verbose (build-spn interpreter-thunk 'max-spn-size max-spn-size))]
          [_ (verbose-pe "SPN # edges: " (hashtable-size (spn->edges spn)) "\n")]
          [_ (verbose-pe "Building equations...\n")]
          [equations (spn-equations spn)]
          [_ (verbose-pe "Solving equations...\n")]
          [solutions (opt-timeit verbose (solve-equations equations))]
          [_ (verbose-pe "done!\n")])
     (lookup-marginals spn solutions)))

 )








