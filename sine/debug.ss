#!r6rs

(library

 (sine debug)

 (export dist->string
         state->string
         stack->string
         subcall->string
         subcall-args->string
         show-stack
         show-slot
         slot->string
         &->string:n
         marginal->string
         show-marginal)

 (import (rnrs)
         (scheme-tools math distributions)
         (scheme-tools hashtable)
         (scheme-tools srfi-compat :1)
         (scheme-tools value-number)
         (scheme-tools)
         (sine coroutine-id)
         (sine coroutine-interpreter)
         (sine syntax))

 ;; --------------------------------------------------------------------
 ;; Debug tools

 (define (dist->string dist)
   (let-values ([(vals ps) (dist-vals&ps dist)])
     (apply string-append
            (vector->list
             (vector-map (lambda (v p) (string-append (&->string:n v 30) ": " (number->string (exp p))))
                         vals
                         ps)))))

 (define (state->string state)
   (cond [(subcall? state) (subcall->string state)]
         [(xrp? state) "xrp"]
         [else (&->string:n state 20)]))

 (define (stack->string stack)
   (string-append "[" (apply string-append (map (lambda (x) (string-append (->string x) " ")) (map subcall-id stack))) "]"))

 (define (subcall->string subcall)
   (subcall-args->string (subcall-args subcall)))

 (define/kw (subcall-args->string args [num-chars :default 80] [show-env :default #f])
   (if (&pair? args)
       (string-append (->string:n (syntax->original-expr (&car args)) num-chars)
                      " "
                      (if show-env
                          (->string (vector-map car (&expand-recursive (&cdr args))))
                          ""))
       (->string:n (&expand-recursive args) num-chars)))

 (define (show-stack stack)
   (pe "stack:\n")
   (for-each (lambda (e i)
               (pe "  " i ": " (subcall->string e) "\n"))
             stack
             (reverse (iota (length stack))))
   (pe "\n"))

 (define (show-slot &slot)
   (pe "slot: " (slot->string &slot) "\n"))

 (define (slot->string &slot)
   (apply string-append
          `("[" ,@(map (lambda (&v) (string-append (&->string:n &v 30) ", "))
                       (&expand-list &slot))
            "]")))

 (define (&->string:n v n)
   (->string:n (&expand-recursive v) n))

 (define (marginal->string marginal)
   (apply string-append
          `("{"
            ,@(alist-map (lambda (v p) (string-append (&->string:n v 30) ", "
                                                 (->string p) ";  "))
                         (hashtable->alist marginal))
            "}"
            )))

 (define (show-marginal marginal)
   (display (marginal->string marginal)))

 )