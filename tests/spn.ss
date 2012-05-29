#!r6rs

(import (rnrs)
        (scheme-tools)
        (sine spn)
        (sine coroutine-interpreter))

(define (expr->spn expr)
  (let* ([interpreter-thunk (lambda () (coroutine-interpreter expr))]
         [spn (build-spn interpreter-thunk)])
    (let-values ([(keys vals) (hashtable-entries (spn->edges spn))])
      (for-each (lambda (k v) (pretty-print (cons k v)))
                (vector->list keys)
                (vector->list vals)))
    ;; (spn-equations spn
    ))

(expr->spn '(list ((lambda (x) (not x)) (flip .1)) (flip .5)))

;; (expr->spn '(flip))

