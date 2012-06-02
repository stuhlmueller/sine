#!r6rs

(import (rnrs)
        (scheme-tools)
        (sine utils)
        (sine spn))

(define islanders-expr
  '(begin

     (define num-agents 3)

     (define baserate 0.05)

     (define agent*
       (lambda (agent get-correctly-raised-hands get-incorrectly-raised-hands run-game)
         (lambda (from prev-raised-hands true-others-blue-eyes)
           (query/cache
            (define my-blue-eyes (nflip baserate))
            (define total-blue-eyes (+ my-blue-eyes true-others-blue-eyes))
            my-blue-eyes
            (and (> total-blue-eyes 0)
                 (and (equal? prev-raised-hands
                              (sum (run-game 0 from 0 0 total-blue-eyes)))
                      (equal? prev-raised-hands
                              (sum (run-game 0 from 0 0 total-blue-eyes)))))))))

     (define get-correctly-raised-hands*
       (lambda (agent get-correctly-raised-hands get-incorrectly-raised-hands run-game)
         (lambda (from prev-raised-hands true-blue-eyes)
           (cache
            (sum-repeat (lambda () (agent from prev-raised-hands (- true-blue-eyes 1)))
                        true-blue-eyes)))))

     (define get-incorrectly-raised-hands*
       (lambda (agent get-correctly-raised-hands get-incorrectly-raised-hands run-game)
         (lambda (from prev-raised-hands true-blue-eyes)
           (cache
            (sum-repeat (lambda () (agent from prev-raised-hands true-blue-eyes))
                        (- num-agents true-blue-eyes))))))

     (define run-game*
       (lambda (agent get-correctly-raised-hands get-incorrectly-raised-hands run-game)
         (lambda (from to prev-raised-hands-correctly prev-raised-hands-incorrectly true-blue-eyes)
           (cache
            (if (>= from to)
                (list prev-raised-hands-correctly
                      prev-raised-hands-incorrectly)
                (let* ([prev-raised-hands (+ prev-raised-hands-correctly prev-raised-hands-incorrectly)]
                       [raised-hands-correctly (get-correctly-raised-hands from prev-raised-hands true-blue-eyes)]
                       [raised-hands-incorrectly (get-incorrectly-raised-hands from prev-raised-hands true-blue-eyes)])
                  (run-game (+ from 1)
                            to
                            raised-hands-correctly
                            raised-hands-incorrectly
                            true-blue-eyes)))))))

     (define fns (Y* agent* get-correctly-raised-hands* get-incorrectly-raised-hands* run-game*))

     (define run-game (fourth fns))

     (define (game steps true-blue-eyes)
       (run-game 0 steps 0 0 true-blue-eyes))

     (game 2 2)

     ))

(let ([marginals (log-marginal->marginal (time (marginalize islanders-expr 'max-spn-size 100000)))])
  (for-each pen marginals)
  (pen "sum: " (sum-of-marginals marginals)))