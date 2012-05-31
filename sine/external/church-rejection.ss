#!r6rs

(library

 (sine external church-rejection)

 (export church-rejection)

 (import (rnrs)
         (scheme-tools)
         (scheme-tools srfi-compat :1)
         (church compiler))

 (define NUM-SAMPLES -1) ;; run indefinitely

 (define compilation-env
   (environment '(except (rnrs) real? negative? positive? zero? >= <= > < = atan cos sin expt log exp sqrt / * - + min)
                '(rnrs mutable-pairs)
                '(church trie)
                '(church AD)
                '(scheme-tools srfi-compat :1)
                '(rename (except (scheme-tools math) lngamma) (sample-discrete discrete-sampler))
                '(rename (only (ikarus) gensym pretty-print exact->inexact void make-parameter parameterize) (gensym scheme-gensym))
                '(srfi :19)
                '(scheme-tools hash)
                '(scheme-tools py-pickle)
                '(church compiler)
                '(rnrs eval)  ))

 (define (make-compilation-expr sexpr)
   `(let ()
      (define (pair->list p)
        (list (car p) (cdr p)))
      (define (*with-score-gradient*) #f)
      (define tapify (lambda (x) x))
      (define untapify (lambda (x) x))
      (define tape? #f)
      (define (min a b) (if (< a b) a b))
      (define (continuous? x) (and (real? x) (not (fixnum? x))))
      (letrec ,(map (lambda (def)
                      (if (symbol? (cadr def))
                          (list (cadr def) (caddr def))
                          `(,(car (cadr def)) (lambda ,(cdr (cadr def)) ,@(cddr def)))))
                    (compile (list sexpr) '()))
        (church-main '() (make-empty-store)))))

 (define (church-eval sexpr)
   (let ([compilation-expr (make-compilation-expr sexpr)])
     ;; (display compilation-expr)
     (eval compilation-expr
           compilation-env)))

 (define (begin-wrap exprs)
   (if (null? (rest exprs))
       (first exprs)
       `(begin ,@exprs)))

 (define (church-rejection source limit)
   (let ([new-source `(repeat ,limit (lambda () ,source))])
     ;; (pretty-print new-source)
     (church-eval new-source)))

 )