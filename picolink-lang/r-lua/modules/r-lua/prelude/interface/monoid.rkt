#lang picolink/r-lua/base

(require (only lua/ext/values  foldl-values)

         (only r-lua/prelude/definition  define)
         (only r-lua/prelude/interface   define-interface))

(provide (all-defined))

(define-interface Monoid
  [mappend %mappend])

(define (<> . [: vs #%values])
  (foldl-values mappend vs))
