#lang picolink/r-lua/base

(require (only lua  set-metatable!)

         (only lua/ext/debug   debug-set-metatable!)
         (only lua/ext/ops    + ->)
         (only lua/ext/table  table-length)

         (only r-lua/runtime/pool/handle
               handle-pool
               handle-chunk
               handle->string)
         (only r-lua/runtime/pool
               pool-debug
               pool-name
               pool-type
               pool-chunk-size
               pool-fields
               pool-field
               pool-retain!
               pool-release!
               pool-chunk->string)
         (only r-lua/runtime/pool/registry get-pool)

         (only r-lua/prelude/binding              let let*)
         (only r-lua/prelude/definition           define)
         (only r-lua/prelude/for                  for)
         (only r-lua/prelude/interface            impl)
         (only r-lua/prelude/interface/copy       Copy copy)
         (only r-lua/prelude/interface/drop       Drop drop)
         (only r-lua/prelude/interface/equal      Equal)
         (only r-lua/prelude/interface/to-string  ToString)
         (only r-lua/prelude/sequence             in-range)
         (only r-lua/prelude/type                 define-type))

(provide copy-handle!
         drop-handle!

         Handle
         Handle?

         Chunk
         Chunk?

         Pooled
         Pooled?)

; Reusable pool type methods

(define (foreach-handle! self f)

  (let* ([pool-id     (handle-pool  self)]
         [chunk-index (handle-chunk self)]
         [pool        (get-pool pool-id)]
         [name        (pool-name       pool)]
         [fields      (pool-fields     pool)]
         [chunk-size  (pool-chunk-size pool)])

    (for ([field-index (in-range 1 (+ (table-length fields) 1))])

      (let ([field (-> fields field-index)])
        (for ([offset (in-range 0 chunk-size)])

          (f pool field chunk-index offset
             (pool-field pool field chunk-index offset)))))))

(define (copy-handle! self)

  (foreach-handle!
   self
   (λ (pool field chunk-index offset value)
     (pool-debug "~a pool copying field ~a chunk ~a offset ~a"
                 (pool-name pool) field chunk-index offset)
     (copy value)))

  (pool-retain! self))

(define (drop-handle! self)

  (foreach-handle!
   self
   (λ (pool field chunk-index offset value)
     (pool-debug "~a pool dropping field ~a chunk ~a offset ~a"
                 (pool-name pool) field chunk-index offset)
     (drop value)))

  (pool-release! self))

; Chunk printer

(define-type Chunk)

(impl [Copy Chunk]
      (define copy copy-handle!))

(impl [Drop Chunk]
      (define drop drop-handle!))

(impl [Equal Chunk])

(impl [ToString Chunk]
      (define to-string pool-chunk->string))

; Handle printer

(define-type Handle)

(impl [Copy Handle]
      (define copy copy-handle!))

(impl [Drop Handle]
      (define drop drop-handle!))

(impl [Equal Handle])

(impl [ToString Handle]
      (define to-string handle->string))

; Pool type dispatcher

(define-type Pooled
  #:index
  (λ (self key)

    (let* ([pool-id (handle-pool self)]
           [pool    (get-pool pool-id)]
           [type    (pool-type pool)]
           [out     (-> (debug-set-metatable! self type) key)])

      (debug-set-metatable! self Handle)

      out)))
