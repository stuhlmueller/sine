#!r6rs

(import (rnrs)
        (only (scheme-tools) symbol-maker pretty-print gensym sum all pe))

(define not-found (gensym))

(define (hashtable-ref/default table key thunk)
  (let ([v (hashtable-ref table key not-found)])
    (if (eq? v not-found)
        (let ([v* (thunk)])
          (hashtable-set! table key v*)
          v*)
        v)))

(define (pretty-print-hashtable ht)
  (let-values ([(keys vals) (hashtable-entries ht)])
    (for-each (lambda (k v) (pretty-print (cons k v)))
              (vector->list keys)
              (vector->list vals))))

;; FIXME: make sure probability of collisions is low
(define (vn-hash obj)
  (cond [(null? obj) 3]
        [(boolean? obj) (if obj 5 7)]
        [(pair? obj) (vn-hash (+ (vn-hash (car obj)) (vn-hash (cdr obj))))]
        [(symbol? obj) (symbol-hash obj)]
        [(number? obj) (mod obj (- (expt 2 29) 3))]
        [(vector? obj) (vn-hash (sum (vector->list (vector-map vn-hash obj))))]
        [else (error obj "cannot hash obj type")]))

(define (vn-equal? obj1 obj2)
  (cond [(eqv? obj1 obj2) #t]
        [(and (pair? obj1) (pair? obj2))
         (and (vn-equal? (car obj1) (car obj2))
              (vn-equal? (cdr obj1) (cdr obj2)))]
        [(and (vector? obj1) (vector? obj2))
         (all (vector->list
               (vector-map (lambda (v1 v2) (vn-equal? obj1 obj2))
                           obj1
                           obj2)))]
        [else #f]))

(define readable-gensym (symbol-maker 'g))

(define vn-store (make-hashtable vn-hash vn-equal?))
(define obj-store (make-eq-hashtable))

(define (node obj type info)
  (hashtable-ref/default vn-store info
                         (lambda ()
                           (let ([id (readable-gensym)])
                             (hashtable-set! obj-store id obj)
                             id))))

(define (parse obj)
  (cond [(null? obj) (node obj 'null #f)]
        [(pair? obj) (node obj 'pair (cons (parse (car obj)) (parse (cdr obj))))]
        [(vector? obj) (node obj 'vec (vector-map parse obj))]
        [(symbol? obj) (node obj 'sym obj)]
        [(number? obj) (node obj 'num obj)]
        [(boolean? obj) (node obj 'bool obj)]
        [else (error obj "parse: unknown object type")]))


;; --------------------------------------------------------------------
;; Usage example
;;
;; We incrementally build up an object and want to repeatedly get the
;; same id if we get the same object and compute ids without walking
;; the object.

;; Problem: vn-cons currently assumes it gets normal objects, but
;; this is not how it is used below. below, it sometimes gets ids.
;;
;; Need to distinguish between the two use cases.
;;
;; - use a particular data type for VN objects?
;; - object store should not store complete objects; e.g., if X = (cons 'a (cons 'b 'c)),
;;   (vn-car X) should get something like (vn-cons 'a #vn9123)
;;   (vn-cdr X) should get #vn9123
;;   (vn-car (vn-cdr X)) should get 'b
;;   i.e., just expose as much as necessary, leave remainder as ids
;;   the reason for this is not efficiency of the object store, but
;;   the fact that we need to know ids of parts of the object, but can't
;;   if we get the original object back

(define (vn-cons/id a b)
  (parse (cons a b)))

(define (vn-vector . elts)
  (parse (apply vector elts)))

(define (vn-car p)
  (car (hashtable-ref obj-store p)))

(define (vn-vector-ref v i)
  (vector-ref (hashtable-ref obj-store v) i))

(define get-id (lambda (x) x))

(define (test)
  (let* ([v1 (vn-cons 1 (vn-cons (vn-vector #f #t) 'foo))]
         [_ (pe "1: " (get-id v1) "\n")]
         [v2 (vn-cons 3 v1)]
         [_ (pe "2: " (get-id v2) " (no complete walk)\n")]
         [v3 (vn-vector v1 v2)]
         [_ (pe "3: " (get-id v3) " (no complete walk)\n")]
         [v1b (vn-cons 1 (vn-cons (vn-vector #f #t) 'foo))]
         [_ (pe "4: " (get-id v1b) " (complete walk, same id as 1)\n")]
         [v2b (vn-cons 3 v1b)]
         [_ (pe "5: " (get-id v2b) " (no complete walk, same id as 2)\n")]
         [v3b (vn-vector v1 v2b)]
         [v3c (vn-vector v1b v2)]
         [v3d (vn-vector v1b v2b)]
         [_ (pe "6: " (get-id v3b) "/" (get-id v3c) "/" (get-id v3d)
                " (no complete walk, same ids as 3)\n")]
         [v4 (vn-car (vn-vector-ref v3b 1))]
         [_ (pe "7: " v4 " (=3; values are accessible)")])
    'done))

(test)

(pretty-print-hashtable vn-store)