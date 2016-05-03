#lang racket

(require rackunit racket/unsafe/chaperones)

(define (id x) x)

(define/contract id-int
  (integer? . -> . integer?)
  id)

(define/contract id-fun
  (any/c . -> . any/c)
  id)

(define/contract id-fun-int
  (integer? . -> . integer?)
  id-fun)

(define/contract id-int-fun
  (any/c . -> . any/c)
  id-int)

(test-case
    "remove only chaperone"
  (check-exn exn:fail:contract? (λ () (id-int 'foo)))
  (check-not-exn (λ () ((remove-impersonators id-int id-int id) 'foo))))

(test-case
    "remove outer chaperone"
  (check-exn exn:fail:contract? (λ () (id-int-fun 'foo)))
  (check-exn exn:fail:contract?
             (λ () ((remove-impersonators id-int-fun id-int-fun id-int) 'foo))))

(test-case
    "remove inner chaperone"
  (check-exn exn:fail:contract? (λ () (id-fun-int 'foo)))
  (check-exn exn:fail:contract?
             (λ () ((remove-impersonators id-fun-int id-fun id) 'foo)))
  (check-exn exn:fail:contract? (λ () (id-int-fun 'foo)))
  (check-not-exn (λ () ((remove-impersonators id-int-fun id-int id) 'foo))))

(test-case
    "check chaperone arguments"
  (check-exn exn:fail:contract? (λ () (remove-impersonators id id-int id)))
  (check-exn exn:fail:contract? (λ () (remove-impersonators id-int id id)))
  (check-exn exn:fail:contract? (λ () (remove-impersonators id-int id-fun id)))
  (check-exn exn:fail:contract? (λ () (remove-impersonators id-int-fun id-int id-fun))))
