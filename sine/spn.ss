#!r6rs

(library

 (sine spn)

 (export marginalize)

 (import (rnrs)
         (scheme-tools)
         (scheme-tools implementation-specific)
         (sine build-spn)
         (sine preamble)
         (sine spn-solve)
         (sine hashtable)
         (sine spn-equations)
         (sine coroutine-interpreter)
         (sine value-number)
         (sine coroutine-id))

 (define verbose #f)

 (define (verbose-pe . args)
   (when verbose
         (apply pe args)))

 (define (marginalize expr)
   (let* ([interpreter-thunk (lambda () (coroutine-interpreter (with-preamble expr)))]
          [_ (verbose-pe "Building SPN...\n")]
          [spn (time (build-spn interpreter-thunk))]
          [_ (verbose-pe "Building equations...\n")]
          [equations (spn-equations spn)]
          [_ (verbose-pe "Solving equations...\n")]
          [solutions (time (solve-equations equations))]
          [_ (verbose-pe "done!\n")])
     (let* ([terminal-ids (hashtable-ref (spn->terminal-ids spn) 'root '())]
            [eqn-ids (map (lambda (terminal-id) (sym-append 'root terminal-id))
                          terminal-ids)])
       (map (lambda (eqn-id terminal-id)
              (let ([logprob (hashtable-ref/nodef solutions eqn-id)]
                    [value (&expand-recursive (terminal-id->value terminal-id))])
                (pair value logprob)))
            eqn-ids
            terminal-ids))))

 )








