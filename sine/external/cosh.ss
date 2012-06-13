#!r6rs

(library

 (sine external cosh)

 (export marginalize)

 (import (cosh components)
         (cosh polymap)
         (rnrs)
         (scheme-tools graph components)
         (scheme-tools graph utils)
         (scheme-tools srfi-compat :1)
         (scheme-tools value-number)
         (scheme-tools)
         (sine coroutine-id)
         (sine coroutine-interpreter)
         (sine polygraph)
         (sine preamble))

 (define (marginalize expr)
   (let* ([interpreter-thunk (lambda () (coroutine-interpreter (with-preamble expr)))]
          [graph (interpreter-thunk->polygraph interpreter-thunk)]
          [polymap (polygraph->polymap graph)]
          [components (strongly-connected-components polymap)]
          [marginals (marginalize-components graph components)]
          [marginal-values (map (compose &expand-recursive terminal-id->value car) marginals)]
          [marginal-probs (map (compose cdr) marginals)])
     (map cons marginal-values marginal-probs)))

 )