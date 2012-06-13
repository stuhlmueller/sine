#!r6rs

(library

 (sine coroutine-id)

 (export recur-id
         recur-id?
         terminal-id
         terminal-id?
         terminal-id->value)

 (import (rnrs)
         (scheme-tools object-id)
         (scheme-tools value-number)
         (scheme-tools)
         (sine coroutine-interpreter))

 (define (recur-id recur)
   (assert (recur? recur))
   (recur-state recur))

 (define (terminal-id terminal)
   (assert (terminal? terminal))
   (terminal-value terminal))

 (define (terminal-id? obj)
   (prefixed-symbol? obj 'ter))

 (define (terminal-id->value id)
   (assert (symbol? id))
   id)

 (define (recur-id? obj)
   (prefixed-symbol? obj 'rec))

 )