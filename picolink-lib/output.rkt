#lang racket/base

(require racket/generic
         racket/contract)

(provide (all-defined-out))

(define-generics output
  (source output)
  (compile output))

(define output-registry
  (make-hash))

(define/contract (register-output name in)
  (-> symbol? (-> syntax? output?) void)
  (hash-set! output-registry name in))

(define/contract (output name)
  (-> symbol? (-> syntax? output?))
  (hash-ref output-registry name))

(define/contract (make-output name source)
  (-> symbol? syntax? output?)
  ((output name) source))
