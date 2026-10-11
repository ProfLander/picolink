#lang racket/base

(provide (all-defined-out))

(struct import [local-id
                src-sym
                src-mod-path
                orig-stx]
  #:transparent)
