#lang scribble/manual
@require[@for-label[bounded
                    racket/base
		    racket/class]]
@require[scribble/eval]

@(define contract-eval
   (lambda ()
     (let ([the-eval (make-base-eval)])
       (the-eval '(require racket/contract racket/class bounded))
       the-eval)))

@title{bounded}
@author{Scott Moore}

@defmodule[bounded]

This module defines contracts that emulate bounded polymorphism.

@defform[(bounded-polymorphic->/c ([x bound] ...) c)]{

Creates a contract for a bounded polymorphic functions.  Each function is
protected by @racket[c], where each @racket[x] is bound in @racket[c] and refers
to a polymorphic type that is instantiated each time the function is applied.

At each application of a function, the @racket[bounded-polymorphic->/c] contract creates a
fresh contract for each type variable @racket[x]. Values flowing into the polymorphic
function (i.e. values protected by some @racket[x] in negative position with
respect to @racket[bounded-polymorphic->/c]) are contracted with the corresponding
@racket[bound] contract. Values flowing out of the polymorphic function
(i.e. values protected by some @racket[x] in positive position with respect to
@racket[bounded-polymorphic->/c]) are checked to ensure they were wrapped by the corresponding @racket[x] contract in a negative position. If so, the @racket[bound] contract is removed
from the value; if not, a contract violation is signaled.

Contracts supplied as bounds must be higher order contracts.

@examples[#:eval (contract-eval)
(define (id x) x)
(define/contract (check fn val)
  (bounded-polymorphic->/c ([X (integer? . -> . integer?)]) (X any/c . -> . X))
  (fn val)
  fn)
(check id 0)
(check id 'bad)
((check id 0) 'ok)
]

To correctly enforce blame, additional contracts applied to values flowing through a bounded contract variable in a negative position are enforced after flowing through the corresponding contract in a positive position.

@examples[#:eval (contract-eval)
(define (id x) x)
(define/contract (coerce-string fn)
  (any/c . -> . (string? . -> . string?))
  fn)
(define/contract (check fn)
  (bounded-polymorphic->/c ([X (integer? . -> . integer?)]) (X . -> . X))
  (coerce-string fn))
((check id) 0)
]

}

@defform/subs[
#:literals (field)

(object/c-opaque member-spec ...)

([member-spec
  method-spec
  (field field-spec ...)]
 
 [method-spec
  method-id
  (method-id method-contract)]
 [field-spec
  field-id
  (field-id contract-expr)])]{
Like @racket[object/c], produces a contract for an object. Unlike @racket[object/c],
fields and methods not named in the contract are not accessible via the contracted object.
This contract was adapted from Typed Racket's object type contracts.

@examples[#:eval (contract-eval)
(define one-method%
  (class object%
    (define/public (foo) 'foo)
    (super-new)))

(define two-method%
  (class one-method%
    (inherit foo)
    (define/public (bar) 'bar)
    (super-new)))

(define one (new one-method%))
(define two (new two-method%))

(define/contract (use-one obj)
  (-> (object/c-opaque (foo (->m symbol?))) any/c)
  (send obj foo)
  obj)
(define/contract (use-two obj)
  (-> (object/c-opaque (foo (->m symbol?))) any/c)
  (send obj bar)
  obj)
(define/contract (use-one-poly obj)
  (bounded-polymorphic->/c
    ([X (object/c-opaque (foo (->m symbol?)))])
    (-> X X))
  (send obj foo)
  obj)
(define/contract (use-two-poly obj)
  (bounded-polymorphic->/c
    ([X (object/c-opaque (foo (->m symbol?)))])
    (-> X X))
  (send obj bar)
  obj)
(use-one-poly one)
(use-one-poly two)
(use-two-poly one)
(use-two-poly two)
(send (use-one two) bar)
(send (use-one-poly two) bar)
]
}
