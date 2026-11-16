#lang picolink/r-lua/base

(require (only lua  to-number)

         (only lua/ext/ops  -> =)

         (only luajit/ffi  cast new)

         (only r-lua/runtime/void        void)

         (only r-lua/prelude/definition  define))

(provide (all-defined))

(define (uint64 n)
  (cast "uint64_t" n))

(define (uint64-array n)
  (new "uint64_t[?]" n))

(define (uint64-array-ref array index)
  (to-number (-> array index)))

(define (uint64-array-set! array index value)
  (= array index (cast "uint64_t" value)))
