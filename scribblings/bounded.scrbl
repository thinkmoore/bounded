#lang scribble/manual
@require[@for-label[bounded
                    racket/base]]
@require[scribble/eval]

@(define contract-eval
   (lambda ()
     (let ([the-eval (make-base-eval)])
       (the-eval '(require racket/contract bounded "private/bounded-impersonator-util.rkt"))
       the-eval)))

@title{bounded}
@author{sdmoore}

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

@defmodule["private/bounded-impersonator-util.rkt"]

@defform[(remove-impersonator value impersonator original)]{

Returns @racket[value] with @racket[impersonator] removed from the
chain of impersonators wrapping @racket[value]. @racket[value] must be
an impersonator of (or equal to) @racket[impersonator]. @racket[impersonator]
must directly impersonate @racket[original], which is provided as a witness
that the caller has access to the impersonated value. (Note: replace witness
requirement with appropriate code-inspector?)

@examples[#:eval (contract-eval)
(define (id x) x)
(define/contract id-int
  (integer? . -> . integer?)
  id)
(define/contract id-int-fun
  (any/c . -> . any/c)
  id-int)
(define removed
  (remove-impersonator id-int-fun id-int id))
(id-int 'bad)
(id-int-fun 'bad)
(removed 'ok)
]
}
