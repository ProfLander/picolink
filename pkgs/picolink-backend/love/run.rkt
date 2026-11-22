#lang racket/base

(provide (all-defined-out))

(define (love-run exe build-directory _linked)
  (subprocess #f #f #f exe build-directory))
