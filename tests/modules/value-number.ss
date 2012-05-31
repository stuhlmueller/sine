#!r6rs

;; We incrementally build up an object and want to repeatedly get the
;; same id if we get the same object and compute ids without walking
;; the object.

(import (rnrs)
        (scheme-tools)
        (scheme-tools profile)
        (sine value-number))

(define (&cons-n n elt lst)
  (if (= n 0)
      lst
      (&cons-n (- n 1)
               elt
               (&cons elt lst))))

(define &a (compress-symbol 'a))

(define (profile n)
  (&cons-n n
           &a
           (compress-null '())))

(define (main)
  (pe "n, time\n")
  (for-each (lambda (n) (begin
                     (parameterize
                      ([obj-store (make-obj-store)]
                       [number-store (make-number-store)])
                      (pe n ", " (get-runtime (lambda () (profile n))) "\n"))))
            (map (lambda (x) (* x 20000)) '(1 2 3 4 5 6 7 8 9 10))))

(main)

