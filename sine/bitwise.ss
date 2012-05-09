#!r6rs

;;; Exact bitwise arithmetic

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College
;;; stone@cs.grinnell.edu

;;; created June 30, 2011
;;; last revised June 30, 2011

;;; Currently, the Ikarus library for exact bitwise arithmetic is
;;; incomplete, lacking six procedures that the standard requires.  This
;;; library provides those procedures.

(library

 (bitwise)

 (export bitwise-not bitwise-and bitwise-ior bitwise-xor bitwise-if
         bitwise-bit-count bitwise-length bitwise-first-bit-set
         bitwise-bit-set? bitwise-copy-bit bitwise-bit-field
         bitwise-copy-bit-field bitwise-arithmetic-shift
         bitwise-arithmetic-shift-left bitwise-arithmetic-shift-right
         bitwise-rotate-bit-field bitwise-reverse-bit-field)

 (import (rnrs base)
         (rnrs lists)
         (rename (rnrs arithmetic bitwise)
                 (bitwise-ior ikarus-bitwise-ior)
                 (bitwise-xor ikarus-bitwise-xor)
                 (bitwise-if ikarus-bitwise-if)
                 (bitwise-copy-bit-field ikarus-bitwise-copy-bit-field)
                 (bitwise-rotate-bit-field
                  ikarus-bitwise-rotate-bit-field)
                 (bitwise-reverse-bit-field
                  ikarus-bitwise-reverse-bit-field)))

 ;; The bitwise-ior procedure returns the exact integer object that is the
 ;; bitwise disjunction ("inclusive or") of its arguments.  If it receives
 ;; only one argument, it returns that argument.  If it receives no
 ;; arguments, it returns 0, which is the identity for bitwise
 ;; disjunction.

 (define bitwise-ior
   (lambda arguments
     (fold-right (lambda (left right)
                   (bitwise-not (bitwise-and (bitwise-not left)
                                             (bitwise-not right))))
                 0
                 arguments)))

 ;; The bitwise-xor procedure returns the exact integer object that is the
 ;; bitwise inequivalence ("exclusive or") of its arguments.  If it
 ;; receives only one argument, it returns that argument.  If it receives
 ;; no arguments, it returns 0, which is the identity for bitwise
 ;; inequivalence.

 (define bitwise-xor
   (lambda arguments
     (fold-right (lambda (left right)
                   (bitwise-ior
                    (bitwise-and left (bitwise-not right))
                    (bitwise-and (bitwise-not left) right)))
                 0
                 arguments)))

 ;; The bitwise-if procedure acts as a multiplexer:  For each bit position
 ;; in the twos-complement representation of its first argument, a bit is
 ;; selected from the corresponding position in the twos-complement
 ;; representation of the second or third argument -- from the second
 ;; argument if the bit in the first argument is a 1, from the third
 ;; argument if that bit is a 0.

 (define bitwise-if
   (lambda (selector fore aft)
     (bitwise-ior (bitwise-and selector fore)
                  (bitwise-and (bitwise-not selector) aft))))

 ;; The bitwise-copy-bit-field procedure takes four arguments.  The first
 ;; is the "target" of the copying operation, and the fourth is the
 ;; source; the second and third (start and end) delimit the bit field to
 ;; be replaced with low-order bits from the source in the result.  This
 ;; procedure presupposes that start and end are non-negative integers and
 ;; that start is less than or equal to end.

 (define bitwise-copy-bit-field
   (lambda (target start end source)
     (bitwise-if (bitwise-and (bitwise-arithmetic-shift-left -1 start)
                              (bitwise-not
                               (bitwise-arithmetic-shift-left -1 end)))
                 (bitwise-arithmetic-shift-left source start)
                 target)))

 ;; The bitwise-rotate-bit-field procedure takes four arguments.  The
 ;; first is the "target" of the rotation operation, the second and third
 ;; (start and end) delimit the bit field to be rotates, and the fourth
 ;; (shift) is the number of positions the field is to be shifted in the
 ;; result.  This procedure presupposes that start, end, and shift are
 ;; non-negative integers and that start is less than or equal to end.
 ;; The rotation moves low-order bits towards the high-order end.

 (define bitwise-rotate-bit-field
   (lambda (target start end shift)
     (if (= start end)
         target
         (let ((real-shift (mod shift (- end start))))
           (if (zero? real-shift)
               target
               (let ((break (- end real-shift)))
                 (let ((fore (bitwise-bit-field target break end))
                       (aft (bitwise-bit-field target start break)))
                   (bitwise-copy-bit-field
                    target
                    start
                    end
                    (bitwise-ior
                     (bitwise-arithmetic-shift-left aft real-shift)
                     fore)))))))))

 ;; The bitwise-reverse-bit-field procedure returns the result obtained
 ;; from its first argument (target) by reversing the order of the bits
 ;; within a field delimited by its second and third arguments (start and
 ;; end).  It presupposes that start and end are non-negative integers and
 ;; that start is less than or equal to end.

 (define bitwise-reverse-bit-field
   (lambda (target start end)
     (let loop ((result target)
                (target-position start)
                (result-position (- end 1)))
       (if (= target-position end)
           result
           (loop (bitwise-copy-bit
                  result
                  result-position
                  (if (bitwise-bit-set? target target-position)
                      1
                      0))
                 (+ target-position 1)
                 (- result-position 1)))))))

;;; copyright (C) 2011 John David Stone

;;; This program is free software.  You may redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation -- either version 3 of the License, or (at
;;; your option) any later version.  A copy of the GNU General Public
;;; License is available on the World Wide Web at

;;;                http://www.gnu.org/licenses/gpl.html

;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY -- without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.