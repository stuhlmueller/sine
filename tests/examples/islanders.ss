#!r6rs

(import (rnrs)
        (scheme-tools)
        (sine utils)
        (sine spn))

(define islanders-expr
  '(begin

     (define num-agents 2)

     (define baserate 0.1)

     (define (agent from prev-raised-hands true-others-blue-eyes)
       (rejection-query
        (define my-blue-eyes (nflip baserate))
        (define total-blue-eyes (+ my-blue-eyes true-others-blue-eyes))
        my-blue-eyes
        (and (> total-blue-eyes 0)
             (equal? prev-raised-hands
                     0
                     ;; (sum (run-game 0 from 0 0 total-blue-eyes))
                     ))))

     (define (get-correctly-raised-hands from prev-raised-hands true-blue-eyes)
       (sum-repeat (lambda () (agent from prev-raised-hands (- true-blue-eyes 1)))
                   true-blue-eyes))

     (define (get-incorrectly-raised-hands from prev-raised-hands true-blue-eyes)
       (sum-repeat (lambda () (agent from prev-raised-hands true-blue-eyes))
                   (- num-agents true-blue-eyes)))

     (define (run-game from to prev-raised-hands-correctly prev-raised-hands-incorrectly  true-blue-eyes)
       (if (>= from to)
           (list prev-raised-hands-correctly
                 prev-raised-hands-incorrectly)
           (let* ([prev-raised-hands (+ prev-raised-hands-correctly prev-raised-hands-incorrectly)]
                  [raised-hands-correctly (get-correctly-raised-hands from prev-raised-hands true-blue-eyes)]
                  [raised-hands-incorrectly (get-incorrectly-raised-hands from prev-raised-hands true-blue-eyes)])
             'done
             ;; (run-game (+ from 1)
             ;;           to
             ;;           raised-hands-correctly
             ;;           raised-hands-incorrectly
             ;;           true-blue-eyes)
             )))

     (define (game steps true-blue-eyes)
       (run-game 0 steps 0 0 true-blue-eyes))

     (game 0 1)

     ))

(for-each pen (log-marginal->marginal (time (marginalize islanders-expr))))
