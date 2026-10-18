#lang racket/base

(require syntax/parse)

(provide (all-defined-out))

(define-literal-set dsl-surface-literals
  #:datum-literals [begin

                     rename

                     require
                     only
                     prefix
                     all-except
                     prefix-all-except

                     provide
                     all-from
                     all-from-except
                     all-defined
                     all-defined-except
                     prefix-all-defined
                     prefix-all-defined-except

                     define
                     define-macro
                     let]
  [])
