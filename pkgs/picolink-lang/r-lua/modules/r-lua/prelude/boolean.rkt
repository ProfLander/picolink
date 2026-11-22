#lang picolink/r-lua/base

(require (only lua/ext/debug debug-set-metatable!)
         (only lua/ext/type  boolean?)

         (only r-lua/runtime/void  void)

         (only r-lua/prelude/function             identity)
         (only r-lua/prelude/if                   if)
         (only r-lua/prelude/interface            impl)
         (only r-lua/prelude/interface/copy       Copy)
         (only r-lua/prelude/interface/drop       Drop)
         (only r-lua/prelude/interface/equal      Equal)
         (only r-lua/prelude/interface/to-string  ToString)
         (only r-lua/prelude/type                 define-type))

(provide (all-defined)
         boolean?)

(define-type Boolean)
(void (debug-set-metatable! #f Boolean))

(impl [Copy     Boolean])
(impl [Drop     Boolean])
(impl [Equal    Boolean])

(impl [ToString Boolean]
      (define (to-string self)
        (if self
            "#t"
            "#f")))
