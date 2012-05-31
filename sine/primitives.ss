#!r6rs

(library

 (sine primitives)

 (export deterministic-primitive-procedures
         stochastic-primitive-procedures
         primitive-constants)

 (import (rnrs)
         (scheme-tools)
         (sine value-number)
         (scheme-tools srfi-compat :1))

 (define deterministic-primitive-procedures
   (list (list 'car &car)
         (list 'cdr &cdr)
         (list 'cons &cons)
         (list 'null? &null?)
         (list 'list &list)
         (list 'list-ref &list-ref)
         (list 'not &not)
         (list 'or &or)
         (list 'and &and)
         (list '< &<)
         (list '> &>)
         (list '= &=)
         (list '+ &+)
         (list '- &-)
         (list '* &*)
         (list '/ &/)))

 (define (&flip source . ?p)
   (let ([p (if (null? ?p) .5 (&expand-number (car ?p)))])
     (let ([&bit (source p)])
       &bit)))

 (define stochastic-primitive-procedures
   (list (list 'flip &flip)))

 (define primitive-constants
   (list (list 'true (compress-boolean #t))
         (list 'false (compress-boolean #f))))

 )

