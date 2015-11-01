#lang racket
 
(provide (all-defined-out)) ; export all defined functions
 
(define-struct state (stack words) #:mutable) ; look up `define-struct`
 
(define (new-state)
  (make-state '() (make-hash))) ; make-state is automagically defined because of `define-struct`

; your implemenation of the semantics

; (push-stack val a-state) returns void
( define (push-stack val a-state) (set-state-stack! a-state (cons val (state-stack a-state))) )
; (pop-stack a-state) returns the popped valued
( define (pop-stack      a-state)
   (let ([rtrn (car (state-stack a-state))])
     (set-state-stack! a-state (cdr (state-stack a-state)))  ; nicer way of doing this?
     rtrn
   )
)
; (dump-stack a-state)
( define (dump-stack     a-state) (write (state-stack a-state)) )

; implement (get-word name a-state) and (set-word name body a-state)
( define (get-word name a-state)      (hash-ref  (state-words a-state) name) )
( define (set-word name body a-state) (hash-set! (state-words a-state) name body) )
