#lang racket/base

(require picolink/syntax)

(provide (all-defined-out))

(struct/cross-phase parsed-module [language
                                   require-scope
                                   module-scope
                                   provide-scope
                                   stx])
