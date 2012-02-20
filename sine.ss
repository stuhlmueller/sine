#!r6rs

(import (rnrs)
        (except (scheme-tools) flip)
        (scheme-tools srfi-compat :1)
        (scheme-tools object-id)
        (delimcc-simple-r6rs))

(define-record-type xrp-cont
  (fields id proc vals probs)
  (protocol
   (lambda (n)
     (lambda (c vs ps) 
       (n (gensym) c vs ps)))))

(define (get-id obj)
  (if (xrp-cont? obj)
      (xrp-cont-id obj)
      obj))

(define (flip)
  (shift f (handler (make-xrp-cont f '(#t #f) '(.5 .5)))))

(define edges '())

(define (store-edge! from to p)
  (set! edges (cons (list (get-id from) (get-id to) p)
                    edges)))

(define (handler obj)
  (when (xrp-cont? obj)
        (for-each (lambda (v p) (store-edge! obj ((xrp-cont-proc obj) v) p))
                  (xrp-cont-vals obj)
                  (xrp-cont-probs obj)))
  obj)

(define (explore thunk)
  (reset (thunk)))

;; --------------------------------------------------------------------

(define (test-program)
  (list (and (flip) (flip)) (flip)))

(explore test-program)

(map pretty-print edges)