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

      (define (cache proc)
        (proc))

      (define nfqp-query
        (lambda (nfqp)
          (cache
           (let ([val (nfqp)])
             (if (car val) ;; test is first
                 ((cdr val))
                 (nfqp-query nfqp))))))

      (define nfqp-query/nocache
        (lambda (nfqp)
          (let ([val (nfqp)])
            (if (car val) ;; test is first
                ((cdr val))
                (nfqp-query/nocache nfqp)))))

      (define repeat
        (lambda (N proc)
          (if (= N 0) '() (cons (proc) (repeat (- N 1) proc)))))

      (define single-map
        (lambda (proc lst)
          (if (null? lst)
              '()
              (pair (proc (first lst))
                    (single-map proc (rest lst))))))

      (define multi-map
        (lambda (proc lsts) ;;takes list of lists and proc of that many arguments.
          (if (null? (first lsts))
              '()
              (pair (apply proc (single-map first lsts))
                    (multi-map proc (single-map rest lsts))))))

      (define (map proc . lsts)
        (if (null? (rest lsts))
            (single-map proc (first lsts))
            (multi-map proc lsts)))

      ;; okmij.org/ftp/Computation/fixed-point-combinators.html
      (define (Y* . fl)
        (map (lambda (f) (f))
             ((lambda (x) (x x))
              (lambda (p)
                (map
                 (lambda (f)
                   (lambda ()
                     (apply f
                            (map
                             (lambda (ff)
                               (lambda y (apply (ff) y)))
                             (p p)))))
                 fl)))))

      (define sum
        (lambda (lst)
          (apply + lst)))

      (define all
        (lambda (lst)
          (if (null? lst)
              #t
              (if (car lst)
                  (all (cdr lst))
                  #f))))

      (define any
        (lambda (lst)
          (if (null? lst)
              #f
              (if (car lst)
                  #t
                  (any (cdr lst))))))

      (define %sum-repeat
        (lambda (proc n s)
          (if (= n 0)
              s
              (%sum-repeat proc
                           (- n 1)
                           (+ s (proc))))))

      (define (sum-repeat proc n)
        (%sum-repeat proc n 0))

      (define (nflip p)
        (if (flip p) 1 0))

      ,expr

      ))

 )