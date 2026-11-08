#lang picolink/r-lua/base

(require (only r-lua/prelude/binding     let)
         (only r-lua/prelude/lambda      λ)
         (only r-lua/prelude/logic       or)
         (only r-lua/prelude/sequencing  begin)
         (only r-lua/prelude/sequence    sequence-null?)
         (only r-lua/prelude/values      values)
         (only r-lua/prelude/void        void))

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
