#!r6rs

(library

 (sine coroutine-id)

 (export subcall-id
         subcall-id?
         terminal-id
         terminal-id?
         terminal-id->value)

 (import (rnrs)
         (scheme-tools object-id)
         (scheme-tools value-number)
         (scheme-tools)
         (sine coroutine-interpreter))

 (define (subcall-id subcall)
   (assert (subcall? subcall))
   (subcall-args subcall))

 (define (terminal-id terminal)
   (assert (terminal? terminal))
   (terminal-value terminal))

 (define (terminal-id? obj)
   (prefixed-symbol? obj 'ter))

 (define (terminal-id->value id)
   (assert (symbol? id))
   id)

 (define (subcall-id? obj)
   (prefixed-symbol? obj 'rec))

 )