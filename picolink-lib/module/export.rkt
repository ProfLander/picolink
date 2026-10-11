#lang racket/base

(provide (all-defined-out))

(struct export [local-id
                out-id
                orig-stx]
  #:transparent)
