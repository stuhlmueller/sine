#!r6rs

(import (rnrs)
        (scheme-tools)
        (sine utils)
        (sine spn))

(define scalar-implicature-expr
  '(begin

     (define uniform-draw car)

     (define (all-p state) (all state))
     (define (some-p state) (any state))
     (define (none-p state) (not (some-p state)))

     (define (baserate) 0.6)

     (define (substate-priors)
       (list (lambda () (flip (baserate)))
             (lambda () (flip (baserate)))
             (lambda () (flip (baserate)))))

     (define (belief actual-state access)
       (map (lambda (ac st pr) (if ac st (pr)))
            access
            actual-state
            (substate-priors)))

     (define (state-prior)
       (map (lambda (p) (p)) (substate-priors)))

     (define (sentence-prior)
       (uniform-draw (list all-p some-p none-p)))

     (define listener*
       (lambda (%listener %speaker)
         (lambda (speaker-access sentence depth)
           (rejection-query
            (define state (state-prior))
            state
            (if (= 0 depth)
                (sentence state)
                (equal? sentence
                        (%speaker speaker-access state (- depth 1)))
                )))))

     (define speaker*
       (lambda (%listener %speaker)
         (lambda (access state depth)
           (rejection-query
            (define s (sentence-prior))
            s
            (equal? (belief state access)
                    (%listener access s depth))))))

     (define listener+speaker (Y* listener* speaker*))

     (define listener (first listener+speaker))

     (define speaker (second listener+speaker))

     (define (num-true state)
       (sum (map (lambda (x) (if x 1 0)) state)))

     (num-true (listener '(#t #t #t) some-p 1))

     ))

(let ([marginals (log-marginal->marginal (time (marginalize scalar-implicature-expr 'max-spn-size 50)))])
  (for-each pen marginals)
  (pen "sum: " (sum-of-marginals marginals)))
