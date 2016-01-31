#lang racket/base

(require racket/contract racket/match)
(require "bounded-impersonator-util.rkt")

(require (for-syntax racket/base syntax/name))

(provide (rename-out [_new-bounded-∃/c new-bounded-∃/c]
                     [_new-bounded-∀/c new-bounded-∀/c])
         bounded-∀∃?
         bounded-polymorphic->/c)

(define (bounded-∀∃-proj ctc)
  (let ([in (bounded-∀∃/c-in ctc)]
        [out (bounded-∀∃/c-out ctc)]
        [pred? (bounded-∀∃/c-pred? ctc)]
        [neg? (bounded-∀∃/c-neg? ctc)]
        [bound (bounded-∀∃/c-bound ctc)])
    (define name (bounded-∀∃/c-name ctc))
    (λ (blame)
      (if (equal? neg? (blame-swapped? blame))
          (λ (val)
            (if (pred? val)
                (out val)
                (raise-blame-error blame val "not ~a: ~e" name val)))
          (λ (val)
            (if (contract-first-order-passes? bound val)
                (in bound blame val)
                (raise-blame-error blame val "failed bound ~a: ~e" (contract-name bound) val)))))))

(define-struct bounded-∀∃/c (in out pred? name neg? bound)
  #:omit-define-syntaxes
  #:property prop:custom-write custom-write-property-proc  
  #:property prop:contract
  (build-contract-property
   #:name (λ (ctc) (bounded-∀∃/c-name ctc))
   #:first-order (λ (ctc) (λ (x) #t))
   #:projection bounded-∀∃-proj
   #:stronger (λ (this that) (equal? this that))))

(define-for-syntax (bounded-∀∃/trans which stx)
  (define name (or (syntax-local-name)
                   (let ([n (syntax-local-infer-name stx)])
                     (string->symbol
                      (format "bounded-∀∃-~a" (or n "unknown"))))))
  (syntax-case stx ()
    [x 
     (identifier? #'x) 
     #`(let ([which (case-lambda
                      [() (#,which '#,name)]
                      [(x) (#,which (or x '#,name))])])
         which)]
    [(f) #`(#,which '#,name)]
    [(f x) #`(#,which (or x '#,name))]
    [(f . x)
     (with-syntax ([app (datum->syntax stx '#%app stx stx)])
       #`(app #,which . x))]))

(define-syntax (_new-bounded-∀/c stx) (bounded-∀∃/trans #'new-bounded-∀/c stx))
(define-syntax (_new-bounded-∃/c stx) (bounded-∀∃/trans #'new-bounded-∃/c stx))

(define (new-bounded-∃/c raw-name bound) (mk raw-name #t bound))
(define (new-bounded-∀/c raw-name bound) (mk raw-name #f bound))

(define-values (bounded-∀∃ bounded-∀∃? bounded-∀∃-accessor)
  (make-impersonator-property 'forall-exists))

(define (get-struct-type val)
  (let-values ([(type skipped) (struct-info val)])
    type))

(define (make-in property)
  (lambda (ctc blame val)
    (let* ([contracted (((contract-projection ctc) blame) val)]
           [wrapped (box #f)]
           [chaperone
            (cond
              [(procedure? val)
               (chaperone-procedure contracted #f bounded-∀∃ #t property (list val contracted wrapped))]
              [(struct? val)
               (chaperone-struct contracted (get-struct-type val) property (list val contracted wrapped))]
              [(vector? val)
               (chaperone-vector contracted
                                 (λ (vec ind get) get)
                                 (λ (vec ind put) put)
                                 bounded-∀∃ #t
                                 property (list val contracted wrapped))]
              [(box? val)
               (chaperone-box contracted
                              (λ (box get) get)
                              (λ (box put) put)
                              bounded-∀∃ #t
                              property (list val contracted wrapped))]
              [(hash? val)
               (chaperone-hash contracted
                               (λ (hash key) (values key (λ (hash key v) v)))
                               (λ (hash key v) (values key v))
                               (λ (hash key) key)
                               (λ (hash key) key)
                               bounded-∀∃ #t
                               property (list val contracted wrapped))]
              [(struct-type? val)
               (chaperone-struct-type contracted
                                      (λ args (values args))
                                      (λ (constructor) constructor)
                                      (λ args (values args))
                                      bounded-∀∃ #t
                                      property (list val contracted wrapped))]
              [(evt? val)
               (chaperone-evt contracted
                              (λ (evt) (values evt (λ (v) v)))
                              bounded-∀∃ #t
                              property (list val contracted wrapped))]
              [(channel? val)
               (chaperone-channel contracted
                                  (λ (ch) (values ch (λ (v) v)))
                                  (λ (ch v) v)
                                  bounded-∀∃ #t
                                  property (list val contracted wrapped))]
              [(continuation-prompt-tag? val)
               (chaperone-prompt-tag contracted
                                     (λ args (values args))
                                     (λ args (values args))
                                     bounded-∀∃ #t
                                     property (list val contracted wrapped))]
              [(continuation-mark-key? val)
               (chaperone-continuation-mark-key contracted
                                                (λ (v) v)
                                                (λ (v) v)
                                                bounded-∀∃ #t
                                                property (list val contracted wrapped))])])
      (set-box! wrapped chaperone)
      chaperone)))

(define (make-out property-accessor)
  (lambda (val)
    (match (property-accessor val)
      [(list orig contracted (box wrapped))
       (let* ([unwrapped (remove-impersonator val wrapped contracted)]
              [uncontracted (remove-impersonator unwrapped contracted orig)])
         uncontracted)])))

(define (mk raw-name neg? bound)
  (unless (and (contract? bound)
               (not (flat-contract? bound)))
    (raise-argument-error (if neg? 'new-bounded-∃/c 'new-bounded-∀/c)
                          "higher-order contract"
                          bound))
  (define name (string->symbol (format "~a/~a" raw-name (if neg? "∃" "∀"))))
  (define-values (property predicate accessor)
    (make-impersonator-property name))
  (let ([constructor (make-in property)]
        [destructor (make-out accessor)])
    (make-bounded-∀∃/c constructor destructor predicate raw-name neg? bound)))

(define-syntax (bounded-polymorphic->/c stx)
  (syntax-case stx ()
    [(_ ([x cx] ...) c)
     (begin
       (for ([x (in-list (syntax->list #'(x ...)))])
         (unless (identifier? x)
           (raise-syntax-error 'bounded-polymorphic->/c 
                               "expected an identifier"
                               stx
                               x)))
       (define dup (check-duplicate-identifier (syntax->list #'(x ...))))
       (when dup (raise-syntax-error
                  'bounded-polymorphic->/c 
                  "duplicate identifier"
                  stx
                  dup))
       #`(make-polymorphic-contract '(x ...)
                                    '([x cx] ...)
                                    (list cx ...)
                                    (lambda (x ...) c)
                                    'c))]))

(define-struct polymorphic-contract [vars binders bound-vals body body-src-exp]
  #:property prop:custom-write custom-write-property-proc
  #:property prop:contract
  (build-contract-property
   #:name
   (lambda (c)
     `(bounded-polymorphic->/c ,(polymorphic-contract-binders c) ,(polymorphic-contract-body-src-exp c)))
   #:projection
   (lambda (c)
     (lambda (orig-blame)
              (define blame (blame-add-context orig-blame #f))
       (define (wrap p)
         ;; values in polymorphic types come in from negative position,
         ;; relative to the poly/c contract
         (define negative? (blame-swapped? blame))
         (define instances
           (for/list ([var (in-list (polymorphic-contract-vars c))]
                      [val (in-list (polymorphic-contract-bound-vals c))])
             (mk var negative? val)))
         (define protector
           (apply (polymorphic-contract-body c) instances))
         (((contract-projection protector) blame) p))

       (lambda (p)
         (unless (procedure? p)
           (raise-blame-error blame p '(expected "a procedure" given: "~e") p))
         (make-keyword-procedure
          (lambda (keys vals . args) (keyword-apply (wrap p) keys vals args))
          (case-lambda
            [() ((wrap p))]
            [(a) ((wrap p) a)]
            [(a b) ((wrap p) a b)]
            [(a b c) ((wrap p) a b c)]
            [(a b c d) ((wrap p) a b c d)]
            [(a b c d e) ((wrap p) a b c d e)]
            [(a b c d e f) ((wrap p) a b c d e f)]
            [(a b c d e f g) ((wrap p) a b c d e f g)]
            [(a b c d e f g h) ((wrap p) a b c d e f g h)]
            [args (apply (wrap p) args)])))))))
