#lang racket/base

(require racket/path
         racket/syntax)

(provide (all-defined-out)
         file-name-from-path)

(struct config [root
                backend
                build-directory]
  #:transparent)

(define (make-config #:root            root
                     #:backend         backend
                     #:build-directory [build-directory
                                        (build-path "build")])
  (config root
          backend
          build-directory))

(define config-name "config.rkt")

(define (config-exists? path)
  (let ([path (build-path path config-name)])
    (and (file-exists? path)
         path)))

(define (find-config [path (current-directory)])
  (let loop ([path (simplify-path (path->complete-path path))])
    (and path
         (let ([found (config-exists? path)])
           (or found
               (let-values ([(base _name _must-be-dir?)
                             (split-path path)])
                 (loop base)))))))

(define (config-ident lctx)
  (datum->syntax lctx '#%config))

(define (syntax-local-config lctx)
  (syntax-local-eval (config-ident lctx)))
