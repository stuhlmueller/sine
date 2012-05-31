#!r6rs

(import (rnrs)
        (scheme-tools)
        (scheme-tools sort)
        (scheme-tools profile)
        (scheme-tools hash)
        (scheme-tools srfi-compat :1)
        (sine church-rejection)
        (rename (sine enum-interpreter)
                (enum-interpreter marginalize-enumerate))
        (rename (sine cosh)
                (marginalize marginalize-cosh-interpreter))
        (rename (sine spn)
                (marginalize marginalize-sine-spn))
        (rename (cosh)
                (cosh marginalize-cosh-compiler)))

(define (normalize-bins/log bins)
  (map pair
       (map first bins)
       (map log (normalize (map (lambda (bin) (exact->inexact (rest bin))) bins)))))

(define (replace expr from to)
  (if (list? expr)
      (map (lambda (e) (replace e from to)) expr)
      (if (eq? expr from)
          to
          expr)))

(define engines
  (list
   ;; (pair 'sine-spn marginalize-sine-spn)
   ;; (pair 'cosh-interpeter marginalize-cosh-interpreter)

   (pair 'rejection (lambda (expr n)
                      (let ([samples (church-rejection (replace expr 'query 'rejection-query) n)])
                        (normalize-bins/log (bin samples)))))
   ;; (pair 'enumerate (lambda (expr n) (marginalize-enumerate expr 'limit n)))
   (pair 'cosh-compiler (lambda (expr n) (marginalize-cosh-compiler (list expr) 'limit n)))
   ))

(define max-runtime 10)

(define engine->name car)

(define engine->evaluator cdr)

(define (probability-mass marginal)
  (exp (apply logsumexp (map cdr marginal))))

(define (alist->hash-table* dist)
  (let ([ht (make-equal-hash-table)])
    (alist-map (lambda (v p)
                 (hash-table-set! ht v p))
               dist)
    ht))

(define (show-return x)
  (pe x "\n")
  x)

(define (compute-error true-dist dist)
  (let ([dist-table (alist->hash-table* dist)])
    (sum
     (alist-map (lambda (v p)
                  (abs (- (exp p)
                          (exp (hash-table-ref/default dist-table
                                                       v
                                                       -inf.0)))))
                true-dist))))

(define (profile/limit engine expr limit true-dist)
  (let-values ([(runtime dist) (get-runtime&value (lambda () ((engine->evaluator engine) expr limit)))])
    (let ([mass (if (null? dist) 0.0 (probability-mass dist))]
          [error (compute-error true-dist dist)])
      (pe ;; (engine->name engine) ", limit " limit ":\n"
       ;; (string-sort (alist-map (lambda (a b) (cons a (exp b))) dist)) "\n"
       ;; "mass: " mass "\n"
       ;; "error: " error "\n"
       ;; "runtime: " runtime "\n\n"
       runtime ", " error "\n"
       )
      (values runtime error))))

(define (profile engine expr true-dist)
  (let loop ([limit 10])
    (let-values ([(runtime error) (profile/limit engine expr limit true-dist)])
      (if (> runtime max-runtime)
          'done
          (loop (* limit 2))))))

(define (profile-all expr true-dist)
  (map (lambda (engine)
         (begin
           (pe "\n"(engine->name engine) "\n")
           (profile engine expr true-dist)))
       engines))


;; --------------------------------------------------------------------
;; Models

(define (get-makelist-expr N)
  `(begin
     (define my-make-list
       (lambda (n)
         (if (= n 0)
             '()
             (cons true (my-make-list (- n 1))))))
     (define y (my-make-list ,N))
     'done))

(define bayesnet-expr
  '(begin
     (define alice 0)
     (define bob 1)
     (define sue 2)
     (define tom 3)

     (define (sample-strength)
       (if (flip .5)
           10
           5))

     (define (lazy person)
       (flip (/ 1 20)))

     (query
      (define strengths
        (repeat 4 sample-strength))
      (define (strength person)
        (list-ref strengths person))
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

      (list (> (strength alice) (strength tom))
            (> (strength bob) (strength sue)))

      (and (equal? (winner (list alice bob) (list tom sue))
                   (list alice bob))
           (equal? (winner (list alice bob) (list tom sue))
                   (list tom sue)))


      ;; ))

      ))

  )

;; (pe "runtime, error")
;; (profile-all bayesnet-expr
;;              '((((2 3) (2 1)) . -8.49659469524612)
;;                (((2 3) (0 3)) . -6.188897673186315)
;;                (((0 1) (2 1)) . -2.4048023585300227)
;;                (((0 1) (0 3)) . -0.0971053364702345)))

(pretty-print (marginalize-cosh-compiler (list bayesnet-expr) 'limit 10000000))