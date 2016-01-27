#lang racket

(require "../private/bounded.rkt")
(require rackunit)

(test-case
    "bound restriction"
  (define (id x) x)
  (define/contract (as-integer-good f)
    (bounded-polymorphic->/c ([X (integer? . -> . integer?)])
                            (-> X X))
    (f 0)
    f)

  (define/contract (as-integer-bad f)
    (bounded-polymorphic->/c ([X (integer? . -> . integer?)])
                            (-> X X))
    (f 'foo)
    f)

  (check-not-exn (λ () (as-integer-good id)))
  (check-exn exn:fail:contract? (λ () (as-integer-bad id))))

(test-case
    "bound-lifted"
  (define (id x) x)
  (define/contract (as-integer-good f)
    (bounded-polymorphic->/c ([X (integer? . -> . integer?)])
                            (-> X X))
    (f 0)
    f)

  (check-not-exn (λ () ((as-integer-good id) 'foo))))

(test-case
    "enforce contracts"
  (define (id x) x)
  (define/contract (coerce-to-string fn)
    ((any/c . -> . any/c) . -> . (string? . -> . string?))
    fn)
  (define/contract (do-coercion fn)
    (bounded-polymorphic->/c ([X (integer? . -> . integer?)])
                            (-> X X))
    (coerce-to-string fn))

  (check-exn exn:fail:contract? (λ () ((do-coercion id) 0))))

(test-case
    "first-order"
  (check-exn exn:fail:contract?
             (λ () (let ()
                     (define/contract (add x y)
                       (bounded-polymorphic->/c ([X integer?])
                                                (-> X X X))
                       (+ x y))
                     (add 0 1)))))
