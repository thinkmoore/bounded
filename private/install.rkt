#lang racket/base

(provide pre-installer)

(require racket/file)
(require dynext/file)
(require dynext/compile)
(require dynext/link)
(require setup/dirs)

(define (pre-installer collections-top-path collection-path)
  (define private-path (build-path collection-path "private"))
  (define src-path (build-path collection-path "src"))
  (define private-include (build-path (find-include-dir) 'up "src/racket/src"))
  (parameterize ((current-directory private-path))
    (define shared-object-target-path (build-path private-path
						  "compiled"
						  "native"
						  (system-library-subpath)))
    (define shared-object-target (build-path shared-object-target-path
					     (append-extension-suffix "bounded-impersonator-util_rkt")))
    (when (file-exists? shared-object-target) (delete-file shared-object-target))
    (define c-source (build-path src-path "bounded-impersonator-util.c"))
    (define object (build-path shared-object-target-path "bounded-impersonator-util.o"))
    (make-directory* shared-object-target-path)
    (compile-extension #f ;; not quiet
                       c-source
                       object
                       (list private-include))
    (link-extension #f ;; not quiet
                    (list object)
                    shared-object-target)))
