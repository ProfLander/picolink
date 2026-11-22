#lang racket/base

(require picolink/syntax)

(provide (all-defined-out))

(struct/cross-phase import [local-id
                            src-sym
                            src-mod-path
                            orig-stx])
