#lang info
(define collection "bounded")
(define homepage "https://thinkmoore.github.io/bounded")
(define deps '("base"
               "rackunit-lib"))
(define build-deps '("scribble-lib" "racket-doc"))
(define scribblings '(("scribblings/bounded.scrbl" ())))
(define test-include-paths '("test"))
(define pkg-desc "Bounded polymorphic contracts")
(define version "0.0")
(define pkg-authors '("Scott Moore"))
