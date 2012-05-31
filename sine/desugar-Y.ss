#!r6rs

(library

 (sine desugar-Y)

 (export desugar-all-Y)

 (import (rnrs)
         (scheme-tools)
         (scheme-tools srfi-compat :1)
         (sine sexpr))

 (define readable-gensym
   (symbol-maker '%rec))

 (define (recursive-fn? sexpr)
   (and (tagged-list? sexpr 'define)
        (symbol? (second sexpr))
        (list? (third sexpr))
        (let ([proc (third sexpr)])
          (and (not (null? proc))
               (eq? (first proc) 'lambda)
               (contains? (free-variables proc self-eval-vars)
                          (second sexpr)
                          eq?)))))

 (define (desugar-recursive-fn sexpr)
   (let ([fn (readable-gensym)])
     `(define ,(second sexpr)
        (Y
         (lambda (,fn)
           ,(replace-free-variable (third sexpr)
                                   (second sexpr)
                                   fn))))))

 (define (desugar-all-Y sexpr)
   (if (list? sexpr)
       (if (recursive-fn? sexpr)
           (map desugar-all-Y (desugar-recursive-fn sexpr))
           (map desugar-all-Y sexpr))
       sexpr))

 )
