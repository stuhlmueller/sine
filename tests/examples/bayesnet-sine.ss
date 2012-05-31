#!r6rs

(import (rnrs)
        (scheme-tools)
        (sine spn))

;; (marginalize '(flip))

(marginalize
 '(begin
    (define alice 0)
    (define bob 1)
    (define sue 2)
    (define tom 3)
    (define (sample-strength)
      (if (flip) 10 5))
    (define strengths
      (repeat 4 sample-strength))
    (define (strength person)
      (list-ref strengths person))
    (define (lazy person)
      (flip (/ 1 3)))
    (define (total-pulling team)
      (apply +
             (map (lambda (person)
                    (if (lazy person)
                        (/ (strength person) 2)
                        (strength person)))
                  team)))
    (define (winner team1 team2)
      (if (< (total-pulling team1) (total-pulling team2))
          team2
          team1))
    (winner (list alice bob) (list sue tom))
    ))



