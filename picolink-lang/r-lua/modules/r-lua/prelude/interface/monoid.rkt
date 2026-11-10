#lang picolink/r-lua/base

(require (only lua/ext/values  foldl-values)

         (only r-lua/prelude/definition  define)
         (only r-lua/prelude/interface   define-interface impl)
         (only r-lua/prelude/print      print)
         )

(provide (all-defined))

(define-interface Monoid
  [mappend __concat
           (λ (a b)
             (s-lua (#%cat a b)))])

(define (<> . [: vs #%values])
  (foldl-values mappend vs))

(impl [Monoid String]
      (define (mappend a b)
        (s-lua (#%cat a b))))
