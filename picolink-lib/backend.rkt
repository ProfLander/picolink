#lang racket/base

(require racket/generic
         racket/contract

         picolink/backend/generic
         picolink/backend/compile-ctx
         picolink/backend/link-ctx)

(provide (all-defined-out)
         (all-from-out picolink/backend/generic)
         (all-from-out picolink/backend/compile-ctx)
         (all-from-out picolink/backend/link-ctx))

(define/contract (backend name)
  (-> symbol?
      (generic-instance/c
       gen:backend
       [output-name (-> backend? symbol?)]
       [compile (or/c (-> backend? compile-ctx? any/c) #f)]
       [link (-> backend? link-ctx? any/c)]
       [run (-> backend? any/c any/c)]))

  (dynamic-require
   (string->symbol (format "picolink/~a" name))
   'backend-inst))
