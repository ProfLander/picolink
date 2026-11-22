#lang racket/base

(require racket/path)

(provide (all-defined-out)
         file-name-from-path)

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
