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
         (list 'cadr &cadr)
         (list 'cdr &cdr)
         (list 'cons &cons)
         (list 'null? &null?)
         (list 'list &list)
         (list 'list-ref &list-ref)
         (list 'not &not)
         (list 'or &or)
         (list 'and &and)
         (list 'equal? &eq?) ;; eq? == equal? in compressed space
         (list '< &<)
         (list '> &>)
         (list '>= &>=)
         (list '<= &<=)
         (list '= &=)
         (list '+ &+)
         (list '- &-)
         (list '* &*)
         (list '/ &/)))

 (define (&flip source . ?p)
   (let ([p (if (null? ?p) .5 (&expand-number (car ?p)))])
     (source (vector &true &false)
             (vector (log p) (log (- 1.0 p))))))

 (define (&uniform-draw source &lst)
   (let* ([lst (&expand-list &lst)]
          [len (length lst)])
     (source (list->vector lst)
             (make-vector len (- (log len))))))

 (define stochastic-primitive-procedures
   (list (list 'flip &flip)
         (list 'uniform-draw &uniform-draw)))

 (define primitive-constants
   (list (list 'true (compress-boolean #t))
         (list 'false (compress-boolean #f))))

 )

