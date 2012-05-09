#!r6rs

;; FIXME: make sure probability of hash collisions is low
;; FIXME: Make &expand-recursive more robust (don't rely on &... symbols not being used elsewhere)
;;
;; All functions that start with & take value numbers as arguments.

(library

 (sine value-number)

 (export &cadddr
         &cddddr
         &caddr
         &cdddr
         &cadr
         &cddr
         &car
         &cdr
         compress-boolean
         compress-list
         compress-number
         compress-pair
         compress-recursive
         compress-symbol
         compress-vector
         &cons
         &expand-boolean
         &expand-list
         &expand-number
         &expand-pair
         &expand-recursive
         &expand-symbol
         &expand-vector
         &id
         &vector
         &vector-ref
         &vector?)

 (import (rnrs)
         (sine hashtable)
         (only (scheme-tools)
               symbol-maker
               prefixed-symbol?
               pretty-print
               gensym
               sum
               all
               pe))

 (define readable-gensym (symbol-maker '&))

 (define not-found (gensym 'not-found))

 (define (not-found? obj) (eq? obj not-found))

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

 (define (flat-obj->num info)
   (hashtable-ref/default number-store
                          info
                          (lambda ()
                            (let ([id (readable-gensym)])
                              (hashtable-set! obj-store id info)
                              id))))

 (define (&value-number? obj)
   (and (symbol? obj)
        (prefixed-symbol? obj '&)))


 ;; --------------------------------------------------------------------
 ;; One-step and recursive compression and expansion

 (define (make-typed-compressor is-type?)
   (lambda (obj)
     (assert (is-type? obj))
     (flat-obj->num obj)))

 (define compress-pair (make-typed-compressor pair?))

 (define compress-vector (make-typed-compressor vector?))

 (define compress-number (make-typed-compressor number?))

 (define compress-symbol (make-typed-compressor symbol?))

 (define compress-boolean (make-typed-compressor boolean?))

 (define (compress-list ns)
   (assert (list? ns))
   (if (null? ns)
       (flat-obj->num '())
       (&cons (car ns) (compress-list (cdr ns)))))

 (define (compress-recursive obj)
   (assert (not (&value-number? obj)))
   (cond [(null? obj) (flat-obj->num '())]
         [(pair? obj) (flat-obj->num (cons (compress-recursive (car obj))
                                           (compress-recursive (cdr obj))))]
         [(vector? obj) (flat-obj->num (vector-map compress-recursive obj))]
         [(symbol? obj) (flat-obj->num obj)]
         [(number? obj) (flat-obj->num obj)]
         [(boolean? obj) (flat-obj->num obj)]
         [else (error obj "compress-recursive: unknown object type")]))

 (define (make-typed-expander is-type?)
   (lambda (n)
     (let ([v (hashtable-ref obj-store n not-found)])
       (when (not (is-type? v))
             (pretty-print n)
             (pretty-print v)
             (pretty-print (&expand-recursive n))
             (pretty-print is-type?)
             (assert (is-type? v)))
       v)))

 (define &expand-pair (make-typed-expander pair?))

 (define &expand-number (make-typed-expander number?))

 (define &expand-symbol (make-typed-expander symbol?))

 (define &expand-vector (make-typed-expander vector?))

 (define &expand-boolean (make-typed-expander boolean?))

 (define (&expand-list n)
   (let ([obj (hashtable-ref obj-store n not-found)])
     (cond [(pair? obj) (cons (car obj) (&expand-list (cdr obj)))]
           [(null? obj) '()]
           [else (error "&expand-list: number refers to non-list object")])))

 (define (&expand-recursive n)
   (let ([obj (hashtable-ref obj-store n not-found)])
     (if (not-found? obj)
         n
         (cond [(symbol? obj) (&expand-recursive obj)]
               [(vector? obj) (vector-map &expand-recursive obj)]
               [(pair? obj) (cons (&expand-recursive (car obj)) (&expand-recursive (cdr obj)))]
               [else obj]))))


 ;; --------------------------------------------------------------------
 ;; Operations on compressed data structures:

 (define (&id n)
   n)

 (define (&cons n1 n2)
   (flat-obj->num (cons n1 n2)))

 (define (&vector . ns)
   (flat-obj->num (list->vector ns)))

 (define (&vector? n)
   (vector? (&expand-vector n)))

 (define (&vector-ref n i)
   (vector-ref (&expand-vector n) i))

 (define (&car n)
   (car (&expand-pair n)))

 (define (&cdr n)
   (cdr (&expand-pair n)))

 (define (&cadr n)
   (car (&expand-pair (&cdr n))))

 (define (&cddr n)
   (cdr (&expand-pair (&cdr n))))

 (define (&caddr n)
   (car (&expand-pair (&cddr n))))

 (define (&cdddr n)
   (cdr (&expand-pair (&cddr n))))

 (define (&cadddr n)
   (car (&expand-pair (&cdddr n))))

 (define (&cddddr n)
   (cdr (&expand-pair (&cdddr n))))

 )