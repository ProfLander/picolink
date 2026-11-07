#lang picolink/r-lua/base

(require r-lua/prelude/arith
         r-lua/prelude/lambda
         r-lua/prelude/logic
         r-lua/prelude/sequencing
         (only r-lua/prelude/sequence sequence-null?)
         r-lua/prelude/values
         r-lua/prelude/void)

(provide (all-defined))

(define-macro for
  [(_ ([seq-id sequence] ...)
      body ...)

   (with-syntax ([(seq-id* ...)
                  (generate-temporaries (attribute seq-id))])

     (syntax/loc this-syntax
       (let ([seq-id* sequence] ...)
         (let loop ([seq-id (seq-id*)] ...)
           (if (or (sequence-null? seq-id) ...)
               (void)
               (begin
                 body ...
                 (loop (seq-id*) ...)))))))])

(define-macro for/fold
  [(_ ([acc-id acc] ...)
      ([seq-id sequence] ...)
      body ...)

   (with-syntax ([(seq-id* ...)
                  (generate-temporaries (attribute seq-id))])

     (syntax/loc this-syntax
       (let ([seq-id* sequence] ...)

         (let loop ([acc-id acc]
                    ...
                    [seq-id (seq-id*)]
                    ...)

           (if (or (sequence-null? seq-id) ...)
               (values acc-id ...)
               (let-values ([(acc-id ...) (begin body ...)])
                 (loop acc-id ...
                       (seq-id*) ...)))))))])
