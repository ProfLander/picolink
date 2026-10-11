#lang racket/base

(provide (all-defined-out))

(struct source-module [language
                       dependencies
                       stx]
  #:transparent)
