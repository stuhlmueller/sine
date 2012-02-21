
;; Delimited continuation operators shift/reset
;; From http://okmij.org/ftp/continuations/implementations.html#delimcc-scheme 
;; Originally delimcc-simple.scm, adapted to Ikarus Scheme

 ; The implementation of ordinary shift/reset derived 
 ; by simplifying multi-prompt shift/reset in delimcc.scm
 ;
 ; Although the present code should work on any R5RS Scheme system,
 ; good performance should be expected only on the systems that implement
 ; call/cc efficiently, such as Chez Scheme, Scheme48, Gambit, Larceny.
 ;
 ; Even on systems that support call/cc only inefficiently,
 ; this implementation has an advantage of not leaking memory.
 ; The captured continuation, reified by shift, corresponds only
 ; to the needed prefix of the full continuation, _even_
 ; if call/cc copies the whole stack. In other words, this implementation
 ; has a so-called JAR hack (see shift-reset.scm in Scheme48 distribution)
 ; built in. Please see the memory-leak test at the end.
(library (delimcc-simple-r6rs)
         (export shift reset)
         (import (rnrs))

         (define go #f)

         ; pstack is a list of k: stack fragments
         (define pstack '())

         ; Execute a thunk in the empty environment -- at the bottom of the stack --
         ; and pass the result, too encapsulated as a thunk, to the
         ; continuation at the top of pstack. The top-most pstack frame is
         ; removed.
         ;
         ; We rely on the insight that the capture of a delimited continuation
         ; can be reduced to the capture of the undelimited one. We invoke 
         ; (go th) to execute the thunk th in the delimited context. 
         ; The call to 'go' is evaluated almost in the empty context
         ; (near the `bottom of the stack'). Therefore,
         ; any call/cc operation encountered during the evaluation of th
         ; will capture at most the context established by the 'go' call, NOT
         ; including the context of go's caller. Informally, invoking (go th)
         ; creates a new stack segment; continuations captured by call/cc
         ; cannot span the segment boundaries, and are hence delimited.
         ; 
         ; This emulation of delimited control is efficient providing that
         ; call/cc is implemented efficiently, with the hybrid heap/stack or
         ; stack segment strategies.


         ;; let push_prompt_aux (p : 'a prompt) (body : unit -> 'a) : 'a =
         ;;   let ek = get_ek () in
         ;;   let pframe = {pfr_mark = p.mark; pfr_ek = ek} in
         ;;   let () = ptop := pframe :: (!ptop) in
         ;;   let res = body () in
         ;;   let () = p.mbox := fun () -> res in
         ;;   raise DelimCCE

         (define (reset* th)
           (call/cc
             (lambda (k)
               (set! pstack (cons k pstack))
               (go th))))			; does not return

         (define (shift* f)
           (call/cc
             (lambda (k)			; stack fragment
               (go 
                 (lambda () 
                   (f 
                     (lambda (v)
                       (call/cc (lambda (k1)
                                  (set! pstack (cons k1 pstack))
                                  (k v))))))))))

         ; ------------------------------- Syntactic sugar

         (define-syntax reset
           (syntax-rules ()
                         ((_ ?e ?f ...) (reset* (lambda () ?e ?f ...)))))

         (define-syntax shift
           (syntax-rules ()
                         ((_ ?k ?e ?f ...) (shift* (lambda (?k) ?e ?f ...)))))


         ; Testing garbage-retention in Petite Chez Scheme
         ; Using guardians
         ; For explanations: http://www.scheme.com/csug/smgmt.html#g2352
         ; This memory leak test is due to Chung-chieh Shan.
         ; This test can be adjusted to run on any other system:
         ; it should loop forever in constant memory. In fact, it was first
         ; written in portable Scheme; guardians were added later.

         ;; (define (test-gc)
         ;;   (let ((g (make-guardian)))
         ;;     (let loop ((junk-identity
         ;;                  (let ((junk (list 'junk)))
         ;;                    (cons junk (reset (shift f f)))))
         ;;                (done 10))
         ;;       (if (zero? done)
         ;;         (begin
         ;;           (collect (collect-maximum-generation)) ; force all collection
         ;;           (display "checking if junk became inacessible:") (newline)
         ;;           (do ((junk-inaccessible (g) (g))) ((not junk-inaccessible))
         ;;             (display "collected junk of size: ")
         ;;             (display junk-inaccessible)
         ;;             (newline)))
         ;;         (begin
         ;;           (g (car junk-identity)) ; register with the guardian
         ;;           (set-cdr! (car junk-identity)
         ;;                     (list (cdr junk-identity)))
         ;;           (loop (cons (cdr (car junk-identity))
         ;;                       (cdr junk-identity)) (- done 1)))))))

         ;; ; Another leak test
         ;; (define (leak-test1-g identity-thunk)
         ;;   (let ((g (make-guardian)))
         ;;     (let loop ((id (lambda (x) x)) (done 10))
         ;;       (if (zero? done)
         ;;         (begin
         ;;           (collect (collect-maximum-generation)) ; force all collection
         ;;           (display "collected pieces of junk: ")
         ;;           (display
         ;;             (do ((junk-inaccessible (g) (g)) (c 0 (+ 1 c)))
         ;;               ((not junk-inaccessible) c)))
         ;;           (newline))
         ;;         (begin
         ;;           (g id) ; register with the guardian
         ;;           (loop (id (identity-thunk)) (- done 1)))))))

         (define (init-delimcc) (let ((v
                                        (call/cc
                                          (lambda (k)
                                            (set! go k)
                                            (k #f)))))
                                  (if v
                                    (let* ((r (v))
                                           (h (car pstack))
                                           (_ (set! pstack (cdr pstack))))
                                      (h r))	; does not return
                                    )))
         (init-delimcc))
