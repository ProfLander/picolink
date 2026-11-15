#lang picolink/r-lua/base

(require (only lua/debug  get-info)

         (only lua/ext/type   function?)
         (only lua/ext/debug  debug-set-metatable!)

         (only r-lua/runtime/void                 void)

         (only r-lua/prelude/format               format)
         (only r-lua/prelude/function             identity)
         (only r-lua/prelude/interface            define-interface impl)
         (only r-lua/prelude/interface/copy       Copy)
         (only r-lua/prelude/interface/drop       Drop)
         (only r-lua/prelude/interface/equal      Equal)
         (only r-lua/prelude/interface/to-string  ToString)
         (only r-lua/prelude/type                 define-type))

(provide (all-defined)
         (rename function? procedure?))

(define-type Procedure)
(debug-set-metatable! identity Procedure)

(impl [Copy Procedure]
      (define copy identity))

(impl [Drop Procedure]
      (define drop void))

(impl [Equal Procedure])

(impl [ToString Procedure]
      (define (to-string self)
        (let ([info (get-info self)])
          (format "procedure~a:~a"
                  (table-ref info "source")
                  (table-ref info "linedefined")))))
