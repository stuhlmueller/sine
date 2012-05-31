#!r6rs

(import (rnrs)
        (scheme-tools)
        (cosh components)
        (cosh polymap)
        (scheme-tools graph components)
        (scheme-tools graph utils)
        (sine polygraph)
        (sine coroutine-id)
        (sine value-number)
        (sine coroutine-interpreter)
        (scheme-tools srfi-compat :1))

(define (marginalize-expr expr)
  (let* ([interpreter-thunk (lambda () (coroutine-interpreter expr))]
         [graph (interpreter-thunk->polygraph interpreter-thunk)]
         [polymap (polygraph->polymap graph)]
         [components (strongly-connected-components polymap)]
         [marginals (marginalize-components graph components)]
         [marginal-values (map (compose &expand-recursive terminal-id->value car) marginals)]
         [marginal-probs (map (compose exp cdr) marginals)])
    ;; (display-graph graph)
    (zip marginal-values marginal-probs)))

(for-each pretty-print
          (marginalize-expr '(list ((lambda (x) (not x)) (flip .1)) (flip .5))))
