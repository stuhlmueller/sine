#!r6rs

(library

 (sine preamble)

 (export with-preamble)

 (import (rnrs))

 (define (with-preamble expr)
   `(begin

      (define Y
        (lambda (f)
          (let ([g (lambda (g)
                     (f (lambda args
                          (apply (g g) args))))])
            (g g))))

      (define nfqp-query
        (lambda (nfqp)
          (let ([val (nfqp)])
            (if (car val) ;; test is first
                ((cdr val))
                (nfqp-query nfqp)))))

      (define repeat
        (lambda (N proc)
          (if (= N 0) '() (cons (proc) (repeat (- N 1) proc)))))

      (define map
        (lambda (proc lst)
          (if (null? lst)
              '()
              (cons (proc (car lst))
                    (map proc (cdr lst))))))

      ,expr

      ))

 )