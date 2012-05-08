#!r6rs

;; FIXME: make sure probability of hash collisions is low

(library

 (sine value-number)

 (export obj->&
         &id
         &val
         &cons
         &car
         &cdr
         &vector
         &vector-ref)

 (import (rnrs)
         (sine hashtable)
         (only (scheme-tools)
               symbol-maker
               pretty-print
               gensym
               sum
               all
               pe))

 (define readable-gensym (symbol-maker 'g))

 (define not-found (gensym 'not-found))

 (define number-store (make-hashtable &hash &equal?))

 (define obj-store (make-eq-hashtable))

 (define (&hash obj)
   (cond [(null? obj) 3]
         [(boolean? obj) (if obj 5 7)]
         [(pair? obj) (&hash (+ (&hash (car obj)) (&hash (cdr obj))))]
         [(symbol? obj) (symbol-hash obj)]
         [(number? obj) (mod obj (- (expt 2 29) 3))]
         [(vector? obj) (&hash (sum (vector->list (vector-map &hash obj))))]
         [else (error obj "cannot hash obj type")]))

 (define (&equal? obj1 obj2)
   (cond [(eqv? obj1 obj2) #t]
         [(and (pair? obj1) (pair? obj2))
          (and (&equal? (car obj1) (car obj2))
               (&equal? (cdr obj1) (cdr obj2)))]
         [(and (vector? obj1) (vector? obj2))
          (all (lambda (x) x)
               (vector->list (vector-map &equal? obj1 obj2)))]
         [else #f]))

 (define (node info)
   (hashtable-ref/default number-store
                          info
                          (lambda ()
                            (let ([id (readable-gensym)])
                              (hashtable-set! obj-store id info)
                              id))))

 (define (obj->& obj)
   (cond [(null? obj) (node '())]
         [(pair? obj) (node (cons (obj->& (car obj)) (obj->& (cdr obj))))]
         [(vector? obj) (node (vector-map obj->& obj))]
         [(symbol? obj) (node obj)]
         [(number? obj) (node obj)]
         [(boolean? obj) (node obj)]
         [else (error obj "&: unknown object type")]))

 (define (&cons n1 n2)
   (node (cons n1 n2)))

 (define (&vector . ns)
   (node (list->vector ns)))

 (define (&car n)
   (car (&val n)))

 (define (&cdr n)
   (cdr (&val n)))

 (define (&vector-ref n i)
   (vector-ref (&val n) i))

 (define (&id n)
   n)

 (define (&val n)
   (hashtable-ref obj-store n not-found))

 )