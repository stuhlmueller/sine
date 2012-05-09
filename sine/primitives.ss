#!r6rs

(library

 (sine primitives)

 (export deterministic-primitive-procedures
         stochastic-primitive-procedures
         primitive-constants)

 (import (rnrs)
         (scheme-tools)
         (scheme-tools srfi-compat :1))

 (define deterministic-primitive-procedures
   (list (list 'car car)
         (list 'cdr cdr)
         (list 'cons cons)
         (list 'null? null?)
         (list 'list list)
         (list 'not not)
         (list 'or (lambda l (any (lambda (x) x) l)))
         (list 'and (lambda l (every (lambda (x) x) l)))
         (list '= =)
         (list '+ +)
         (list '- -)
         (list '* *)
         (list '/ /)))

 (define (flip source . ?p)
   (let ([p (if (null? ?p) .5 (car ?p))])
     (let ([bit (source p)])
       bit)))

 (define stochastic-primitive-procedures
   (list (list 'flip flip)))

 (define primitive-constants
   (list (list 'true #t)
         (list 'false #f)))

 )

