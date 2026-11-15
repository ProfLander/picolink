#lang picolink/r-lua/base

(require (only lua/ext/type    number?)
         (only lua/ext/ops     + - * / ^ % < <=)
         (only lua/ext/debug   debug-set-metatable!)
         (only lua/ext/table   table-length table-ref)

         (only r-lua/runtime/pool
               pool-debug
               pool-name
               pool-chunk-size
               pool-fields
               pool-field
               pool-retain!
               pool-release!
               pool-chunk->string)
         (only r-lua/runtime/pool/handle
               handle
               handle?
               handle-pool
               handle-chunk
               handle->string)
         (only r-lua/runtime/pool/registry get-pool)
         (only r-lua/runtime/void                 void)

         (only r-lua/prelude/format               format)
         (only r-lua/prelude/interface            impl)

         (only r-lua/prelude/interface/arith
               Add
               Subtract
               Negate
               Multiply
               Divide
               Exponent
               Modulo)

         (only r-lua/prelude/interface/copy       Copy)
         (only r-lua/prelude/interface/drop       Drop drop)
         (only r-lua/prelude/interface/equal      Equal)
         (only r-lua/prelude/interface/order      Order)
         (only r-lua/prelude/interface/to-string  ToString)

         (only r-lua/prelude/binding              let let*)
         (only r-lua/prelude/definition           define)
         (only r-lua/prelude/for                  for)
         (only r-lua/prelude/if                   if)
         (only r-lua/prelude/sequence             in-range)
         (only r-lua/prelude/type                 define-type))

(provide (all-defined)
         number?)

(define-type Number)
(debug-set-metatable! 0 Number)

(define copy-handle! pool-retain!)

(define (drop-handle! handle)

  (let* ([pool-id     (handle-pool  handle)]
         [chunk-index (handle-chunk handle)]
         [pool        (get-pool pool-id)]
         [name        (pool-name       pool)]
         [fields      (pool-fields     pool)]
         [chunk-size  (pool-chunk-size pool)])

    (for ([field-index (in-range 1 (+ (table-length fields)
                                      1))])

      (let ([field (table-ref fields field-index)])
        (for ([offset (in-range 0 chunk-size)])

          (pool-debug "pool ~a drop field ~a chunk ~a offset ~a"
                      name field chunk-index offset)

          (drop (pool-field pool field chunk-index offset)))))

    (pool-release! handle)))

(impl [Copy Number]
      (define (copy self)
        (if (handle? self)
            (copy-handle! self)
            self)))

(impl [Drop Number]
      (define (drop self)
        (if (handle? self)
            (drop-handle! self)
            (void))))

(impl [Equal    Number])

(impl [Order    Number]

      (define (less-than? self other)
        (if (handle? self)
            (error "Handle does not implement Order")
            (< self other)))

      (define (less-or-equal? self other)
        (if (handle? self)
            (error "Handle does not implement Order")
            (<= self other))))

(impl [Add      Number]
      (define (add self other)
        (if (handle? self)
            (error "Handle does not implement Add")
            (+ self other))))

(impl [Subtract Number]
      (define (subtract self other)
        (if (handle? self)
            (error "Handle does not implement Add")
            (- self other))))

(impl [Negate   Number]
      (define (negate self)
        (if (handle? self)
            (error "Handle does not implement Negate")
            (- self))))

(impl [Multiply Number]
      (define (multiply self other)
        (if (handle? self)
            (error "Handle does not implement Multiply")
            (* self other))))

(impl [Divide   Number]
      (define (divide self other)
        (if (handle? self)
            (error "Handle does not implement Divide")
            (/ self other))))

(impl [Exponent Number]
      (define (expt self other)
        (if (handle? self)
            (error "Handle does not implement Divide")
            (^ self other))))

(impl [Modulo   Number]
      (define (modulo self other)
        (if (handle? self)
            (error "Handle does not implement Modulo")
            (% self other))))

(impl [ToString Number]
      (define (to-string self)
        (if (handle? self)
            (pool-chunk->string self)
            self)))
