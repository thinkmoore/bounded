#lang racket

(require "../private/bounded.rkt"
         "../private/opaque-object.rkt")
(require rackunit)

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

(test-case
    "object/c-opaque"
  (define/contract (use-one obj)
    (-> (object/c-opaque (foo (->m symbol?))) any/c)
    (send obj foo))

  (define/contract (use-two obj)
    (-> (object/c-opaque (foo (->m symbol?))) any/c)
    (send obj bar))

  (check-not-exn (λ () (use-one one)))
  (check-not-exn (λ () (use-one two)))
  (check-exn exn:fail:object? (λ () (use-two one)))
  (check-exn exn:fail:contract? (λ () (use-two two))))

(test-case
    "bounds"
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
  (check-not-exn (λ () (use-one-poly one)))
  (check-not-exn (λ () (use-one-poly two)))
  (check-exn exn:fail:object? (λ () (use-two-poly one)))
  (check-exn exn:fail:contract? (λ () (use-two-poly two)))
  (check-exn exn:fail:contract? (λ () (send (use-one two) bar)))
  (check-not-exn (λ () (send (use-one-poly two) bar))))
