#!r6rs

(import (rnrs)
        (scheme-tools)
        (sine spn))

;; (marginalize '(flip))

(define (get-makelist-expr N)
  `(begin
     (define my-make-list
       (lambda (n)
         (if (= n 0)
             '()
             (cons true (my-make-list (- n 1))))))
     (define y (my-make-list ,N))
     'done))

(pe
 (time
  (marginalize
   (get-makelist-expr 100000))))



