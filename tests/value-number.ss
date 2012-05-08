#!r6rs

;; We incrementally build up an object and want to repeatedly get the
;; same id if we get the same object and compute ids without walking
;; the object.

(import (rnrs)
        (scheme-tools)
        (sine value-number))

(define (test)
  (let* ([v1 (&cons (obj->& 1) (&cons (&vector (obj->& #f) (obj->& #t)) (obj->& 'foo)))]
         [_ (pe "1: " (&id v1) "\n")]
         [v2 (&cons (obj->& 3) v1)]
         [_ (pe "2: " (&id v2) " (no complete walk)\n")]
         [v3 (&vector v1 v2)]
         [_ (pe "3: " (&id v3) " (no complete walk)\n")]
         ;; [_ (pe "3b: " (obj->& #t) "\n")]
         [v1b (&cons (obj->& 1) (&cons (&vector (obj->& #f) (obj->& #t)) (obj->& 'foo)))]
         [_ (pe "4: " (&id v1b) " (complete walk, same id as 1)\n")]
         [v2b (&cons (obj->& 3) v1b)]
         [_ (pe "5: " (&id v2b) " (no complete walk, same id as 2)\n")]
         [v3b (&vector v1 v2b)]
         [v3c (&vector v1b v2)]
         [v3d (&vector v1b v2b)]
         [_ (pe "6: " (&id v3b) "/" (&id v3c) "/" (&id v3d)
                " (no complete walk, same ids as 3)\n")]
         [v4 (&car (&vector-ref v3b 1))]
         [_ (pe "7: " (&val v4) " (=3; values are accessible)\n")])
    'done))

(test)

