#lang racket/base

; library-local exceptions

(require racket/string)

(provide (all-defined-out))

(define (exn:fail:dynamic-require:unknown-module? ex)
  (and (exn:fail:contract? ex)
       (string-contains? (exn-message ex)
                         "dynamic-require: unknown module")))
