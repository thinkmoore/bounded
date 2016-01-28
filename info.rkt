#lang info
(define collection "bounded")
(define homepage "https://thinkmoore.github.io/bounded")
(define deps '("base"
               "rackunit-lib"))
(define build-deps '("scribble-lib" "racket-doc"))
(define pre-install-collection "private/install.rkt")
(define compile-omit-files '("private/install.rkt"))
(define scribblings '(("scribblings/bounded.scrbl" ())))
(define test-include-paths '("test"))
(define pkg-desc "Description Here")
(define version "0.0")
(define pkg-authors '("Scott Moore"))
