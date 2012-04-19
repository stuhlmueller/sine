#!r6rs

(library

 (sine coroutine-id)

 (export recur-id
         recur-id?
         terminal-id
         terminal-id?
         terminal-id->value)

 (import (rnrs)
         (sine coroutine-interpreter)
         (scheme-tools object-id)
         (scheme-tools))

 (define (recur-id recur)
   (assert (recur? recur))
   (sym+num 'rec (object->id (recur-state recur))))

 (define (terminal-id terminal)
   (assert (terminal? terminal))
   (sym+num 'ter (object->id (terminal-value terminal))))

 (define (terminal-id? obj)
   (prefixed-symbol? obj 'ter))

 (define (terminal-id->value id)
   (assert (symbol? id))
   (id->object (sym+num->num id)))

 (define (recur-id? obj)
   (prefixed-symbol? obj 'rec))

 )