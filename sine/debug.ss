#!r6rs

(library

 (sine debug)

 (export recur->string
         recur-state->string
         show-stack
         show-slot
         slot->string
         &->string:n
         marginal->string
         show-marginal)

 (import (rnrs)
         (scheme-tools)
         (scheme-tools srfi-compat :1)
         (sine syntax)
         (sine coroutine-interpreter)
         (sine coroutine-id)
         (sine value-number))

 ;; --------------------------------------------------------------------
 ;; Debug tools

 (define (recur->string recur)
   (recur-state->string (recur-state recur)))

 (define/kw (recur-state->string state [num-chars :default 80])
   (if (&pair? state)
       (->string:n (syntax->original-expr (&car state)) num-chars)
       (->string:n (&expand-recursive state) num-chars)))

 (define (show-stack stack)
   (pe "stack:\n")
   (for-each (lambda (e i)
               (pe "  " i ": " (recur->string e) "\n"))
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
                         (vector->list marginal))
            "}"
            )))

 (define (show-marginal marginal)
   (display (marginal->string marginal)))

 )