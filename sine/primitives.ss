#!r6rs

(library

 (sine primitives)

 (export deterministic-primitive-procedures
         stochastic-primitive-procedures
         primitive-constants)

 (import (rnrs)
         (scheme-tools srfi-compat :1)
         (scheme-tools value-number)
         (scheme-tools))

 (define deterministic-primitive-procedures
   (list (list 'car &car)
         (list 'first &car)
         (list 'cadr &cadr)
         (list 'second &cadr)
         (list 'caddr &caddr)
         (list 'third &caddr)
         (list 'cadddr &cadddr)
         (list 'fourth &cadddr)
         (list 'display (lambda (n) (begin (pen (&expand-recursive n)) &false)))
         (list 'cdr &cdr)
         (list 'rest &cdr)
         (list 'cons &cons)
         (list 'pair &cons)
         (list 'null? &null?)
         (list 'list &list)
         (list 'list-ref &list-ref)
         (list 'not &not)
         (list 'or &or)
         (list 'and &and)
         (list 'eq? &eq?) ;; eq? == equal? in compressed space
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

 (define (&multinomial source &vs &ps)
   (let* ([ps (&expand-recursive &ps)]
          [vs (&expand-list &vs)])
     (source (list->vector vs)
             (list->vector (map log ps)))))

 (define stochastic-primitive-procedures
   (list (list 'flip &flip)
         (list 'uniform-draw &uniform-draw)
         (list 'multinomial &multinomial)))

 (define primitive-constants
   (list (list 'true &true)
         (list 'false &false)))

 )

