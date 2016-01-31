#lang racket/base

; This code taken and modified from the Typed Racket code base

(require racket/class
         racket/match
         racket/contract/base
         racket/contract/combinator
         (for-syntax racket/base syntax/parse))

(provide object/c-opaque)

;; projection for base-object/c-opaque
(define ((object/c-opaque-proj ctc) blame)
  (λ (obj)
    (match-define (base-object/c-opaque
                   base-ctc
                   methods method-ctcs
                   fields field-ctcs)
      ctc)
    (when (not (object? obj))
      (raise-blame-error blame obj "expected an object got ~a" obj))
    (define actual-fields (field-names obj))
    (define actual-methods
      (interface->method-names (object-interface obj)))
    (define remaining-fields
      (remove* fields actual-fields))
    (define remaining-methods
      (remove* methods actual-methods))
    (define guard/c
      (dynamic-object/c (append methods remaining-methods)
                        (append method-ctcs
                                (for/list ([m remaining-methods])
                                  (restrict-hidden->/c)))
                        (append fields remaining-fields)
                        (append field-ctcs
                                (for/list ([m remaining-fields])
                                  (restrict-hidden-field/c obj m)))))
    ;; FIXME: this is a bit sketchy because we have to construct
    ;;        a contract that depends on the actual object that we got
    ;;        since we don't know its methods beforehand
    (((contract-projection guard/c) blame) obj)))

(struct base-object/c-opaque
  (obj/c ; keep a copy of the normal object/c for first-order checks
   method-names method-ctcs field-names field-ctcs)
  #:property prop:contract
  (build-contract-property
   #:first-order (λ (ctc)
                   (define obj/c (base-object/c-opaque-obj/c ctc))
                   (λ (val)
                     (contract-first-order-passes? obj/c val)))
   #:projection object/c-opaque-proj))

(begin-for-syntax
  (define-syntax-class object/c-clause
    #:literals (field)
    #:attributes (method-names method-ctcs field-names field-ctcs)
    (pattern (field [name:id ctc:expr] ...)
             #:with field-names #'(list (quote name) ...)
             #:with field-ctcs #'(list ctc ...)
             #:with method-names #'null
             #:with method-ctcs #'null)
    (pattern [name:id ctc:expr]
             #:with field-names #'null
             #:with field-ctcs #'null
             #:with method-names #'(list (quote name))
             #:with method-ctcs #'(list ctc))))

(define-syntax (object/c-opaque stx)
  (syntax-parse stx
    [(_ ?clause:object/c-clause ...)
     (syntax/loc stx
       (let ([names  (append ?clause.method-names ...)]
             [ctcs   (append ?clause.method-ctcs ...)]
             [fnames (append ?clause.field-names ...)]
             [fctcs  (append ?clause.field-ctcs ...)])
         (base-object/c-opaque
          (dynamic-object/c names ctcs fnames fctcs)
          names ctcs fnames fctcs)))]))

(define (((restrict-hidden->-projection ctc) blame) val)
  (chaperone-procedure val
                       (make-keyword-procedure
                        (λ (_ kw-args . rst)
                          (raise-blame-error (blame-swap blame) val
                                             "cannot call hidden method"))
                        (λ args
                          (raise-blame-error (blame-swap blame) val
                                             "cannot call hidden method")))))

(struct restrict-hidden->/c ()
  #:property prop:chaperone-contract
  (build-chaperone-contract-property
   #:name (λ (ctc) '<hidden-method>) ; FIXME
   #:projection restrict-hidden->-projection))

(define (restrict-hidden-field-proj ctc)
  (define name (restrict-hidden-field/c-name ctc))
  (λ (*blame)
    (define blame
      ;; Blame has been swapped if this is for a set-field!, in which case
      ;; the blame matches the original negative party. Otherwise we want
      ;; to swap to blame negative.
      (if (blame-swapped? *blame)
          *blame
          (blame-swap *blame)))
    (λ (val)
      (raise-blame-error
       blame val
       "cannot read or write hidden field"))))

(struct restrict-hidden-field/c (obj name)
  #:property prop:flat-contract
  (build-flat-contract-property
   #:name (λ (ctc) '<hidden-field>) ; FIXME
   #:projection restrict-hidden-field-proj))
