#!r6rs

(library

 (sine external cosh)

 (export marginalize)

 (import (rnrs)
         (scheme-tools)
         (cosh components)
         (cosh polymap)
         (scheme-tools graph components)
         (scheme-tools graph utils)
         (sine polygraph)
         (sine coroutine-id)
         (sine value-number)
         (sine preamble)
         (sine coroutine-interpreter)
         (scheme-tools srfi-compat :1))

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