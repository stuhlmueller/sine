#!r6rs

(import (rnrs)
        (scheme-tools)
        (sine build-spn)
        (sine spn-solve)
        (sine hashtable)
        (sine spn-equations)
        (sine coroutine-interpreter))

(define (expr->spn expr)
  (let* ([interpreter-thunk (lambda () (coroutine-interpreter expr))]
         [spn (build-spn interpreter-thunk)])
    ;; (let-values ([(keys vals) (hashtable-entries (spn->edges spn))])
    ;;   (for-each (lambda (k v) (pretty-print (cons k v)))
    ;;             (vector->list keys)
    ;;             (vector->list vals)))
    ;; (pe "\n")
    (let ([equations (spn-equations spn)])
      (for-each pretty-print equations)
      (pretty-print-hashtable (solve-equations equations)))))

;; (expr->spn '(list ((lambda (x) (not x)) (flip .1)) (flip .5)))

(expr->spn '(flip))

