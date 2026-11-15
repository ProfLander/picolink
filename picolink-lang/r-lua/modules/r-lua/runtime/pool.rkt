#lang picolink/r-lua/base

(require (only lua/ext/ops  = + - * <= ..)
         (only lua/ext/table
               array-table
               table-ref
               table-set!
               table-length)

         (only luajit/ext/table  make-array-table)
         (only luajit/ext/ffi
               uint64-array
               uint64-array-ref
               uint64-array-set!)

         (only r-lua/runtime/print          print)
         (only r-lua/runtime/pool/handle    handle unpack-handle)
         (only r-lua/runtime/pool/registry  register-pool! get-pool)

         (only r-lua/prelude/binding        let let* let-values)
         (only r-lua/prelude/condition      when unless)
         (only r-lua/prelude/definition     define)
         (only r-lua/prelude/error          error)
         (only r-lua/prelude/format         format)
         (only r-lua/prelude/if             if)
         (only r-lua/prelude/for            for for/fold)
         (only r-lua/prelude/sequence       in-range)
         (only r-lua/prelude/table          table))

(provide pool-debug
         make-pool
         pool-id
         pool-name
         pool-type
         pool-chunk-size
         pool-chunk-count
         pool-fields
         pool-field
         pool-allocate!
         pool-retain!
         pool-release!
         pool-chunk->string
         pool-value->string)

; Debugging

(define debug #t)

(define (pool-debug fmt . [: vs #%values])
  (when debug
    (print (format fmt vs))))

; Member data

(define make-free-list   uint64-array)
(define free-list-ref    uint64-array-ref)
(define free-list-set!   uint64-array-set!)

(define make-generations uint64-array)
(define generations-ref  uint64-array-ref)
(define generations-set! uint64-array-set!)

(define make-ref-counts  uint64-array)
(define ref-counts-ref   uint64-array-ref)
(define ref-counts-set!  uint64-array-set!)

; Constructor

(define (make-pool name type chunk-size chunk-count
                   . [: fields array-table])

  (let* ([capacity     (* chunk-size chunk-count)]
         [capacity-    (- capacity 1)])

    (let ([free-list   (make-free-list   chunk-count)]
          [generations (make-generations chunk-count)]
          [ref-counts  (make-ref-counts  chunk-count)])

      (for ([i (in-range 0 capacity)])
        (generations-set! generations i 1)
        (ref-counts-set!  ref-counts  i 0))

      (let ([pool (table [name        name]
                         [type        type]
                         [fields      fields]

                         [id          -1]
                         [chunk-size  chunk-size]
                         [chunk-count chunk-count]

                         [free-top    -1]
                         [free-list   free-list]

                         [generations generations]
                         [ref-counts  ref-counts]

                         [top         -1])])

        (for ([i (in-range 1 (+ (table-length fields) 1))])
          (let ([key (table-ref fields i)])

            (pool-debug
             "~a pool allocating ~a chunks of size ~a for field ~a"
             name chunk-count chunk-size key)

            (table-set! pool key (make-array-table capacity-))))

        (register-pool! pool)

        pool))))

; Core getters and setters

(define (pool-name pool)
  (table-ref pool "name"))

(define (pool-type pool)
  (table-ref pool "type"))

(define (pool-fields pool)
  (table-ref pool "fields"))

(define (pool-id pool)
  (table-ref pool "id"))

(define (pool-chunk-size pool)
  (table-ref pool "chunk-size"))

(define (pool-chunk-count pool)
  (table-ref pool "chunk-count"))

(define (pool-free-top pool)
  (table-ref pool "free-top"))

(define (pool-set-free-top! pool free-top)
  (table-set! pool "free-top" free-top))

(define (pool-free-list pool)
  (table-ref pool "free-list"))

(define (pool-free pool free-index)
  (free-list-ref (pool-free-list pool) free-index))

(define (pool-set-free! pool free-index free)
  (free-list-set! (pool-free-list pool) free-index free))

(define (pool-generations pool)
  (table-ref pool "generations"))

(define (pool-generation pool chunk-index)
  (generations-ref (pool-generations pool) chunk-index))

(define (pool-set-generation! pool chunk-index generation)
  (generations-set! (pool-generations pool) chunk-index generation))

(define (pool-ref-counts pool)
  (table-ref pool "ref-counts"))

(define (pool-ref-count pool chunk-index)
  (ref-counts-ref (pool-ref-counts pool) chunk-index))

(define (pool-set-ref-count! pool chunk-index count)
  (ref-counts-set! (pool-ref-counts pool) chunk-index count))

(define (pool-top pool)
  (table-ref pool "top"))

(define (pool-set-top! pool top)
  (table-set! pool "top" top))

(define (pool-field-chunks pool field)
  (table-ref pool field))

(define (pool-set-field-chunks! pool field chunks)
  (table-set! pool field chunks))

(define (pool-value-index pool chunk-index value-index)

  (let* ([chunk-size (pool-chunk-size pool)]
         [chunk-offset (* chunk-index chunk-size)])

    (+ chunk-offset value-index)))

(define (pool-field pool field chunk-index [value-index 0])

  (let ([chunks (pool-field-chunks pool field)]
        [value-index (pool-value-index pool chunk-index value-index)])

    (table-ref chunks value-index)))

(define (pool-set-field! pool field chunk-index value-index value)

  (let* ([chunks (pool-field-chunks pool field)]
         [value-index (pool-value-index pool chunk-index value-index)])

    (table-set! chunks value-index value)))

; Validation

(define (handle-valid? handle)
  (let-values ([(pool-id chunk-index generation) (unpack-handle handle)])
    (let ([pool (get-pool pool-id)])
      (= (pool-generation pool chunk-index) generation))))

(define (validate-handle handle)
  (unless (handle-valid? handle)
    (error "generation mismatch")))

; Backend effects

(define (pool-increment-free-top! pool)
  (let* ([free-top (pool-free-top pool) ]
         [new-free-top (+ free-top 1)])

    (pool-debug "~a pool incrementing free-top from ~a to ~a"
                (pool-name pool) free-top new-free-top)

    (pool-set-free-top! pool new-free-top)
    new-free-top))

(define (pool-decrement-free-top! pool)
  (let* ([free-top (pool-free-top pool) ]
         [new-free-top (- free-top 1)])

    (pool-debug "~a pool decrementing free-top from ~a to ~a"
                (pool-name pool) free-top new-free-top)

    (pool-set-free-top! pool new-free-top)
    new-free-top))

(define (pool-return-free-index! pool free)
  (let ([free-index (pool-increment-free-top! pool)])

    (pool-debug "~a pool returning index ~a to free list at ~a"
                (pool-name pool) free free-index)

    (pool-set-free! pool free-index free)))

(define (pool-take-free-index! pool)
  (let* ([free-top (pool-free-top pool)]
         [free (pool-free pool free-top)])

    (pool-debug "~a pool taking index ~a from free list at ~a"
                (pool-name pool) free free-top)

    (pool-decrement-free-top! pool)
    free))

(define (pool-increment-generation! pool chunk-index)

  (let* ([generation (pool-generation pool chunk-index)]
         [new-generation (+ generation 1)])

    (pool-debug
     "~a pool incrementing chunk ~a generation from ~a to ~a"
     (pool-name pool) chunk-index generation new-generation)

    (pool-set-generation! pool chunk-index new-generation)
    new-generation))

(define (pool-increment-ref-count! pool chunk-index)

  (let* ([ref-count (pool-ref-count pool chunk-index)]
         [new-ref-count (+ ref-count 1)])

    (pool-debug "~a pool incrementing chunk ~a ref count from ~a to ~a"
                (pool-name pool) chunk-index ref-count new-ref-count)

    (pool-set-ref-count! pool chunk-index new-ref-count)
    new-ref-count))

(define (pool-decrement-ref-count! pool chunk-index)

  (let* ([ref-count (pool-ref-count pool chunk-index)]
         [new-ref-count (- ref-count 1)])

    (pool-debug "~a pool decrementing chunk ~a ref count from ~a to ~a"
                (pool-name pool) chunk-index ref-count new-ref-count)

    (pool-set-ref-count! pool chunk-index new-ref-count)
    new-ref-count))

(define (pool-increment-top! pool)

  (let* ([name    (pool-name pool)]
         [top     (pool-top pool)]
         [new-top (+ top 1)])

    (pool-debug "~a pool incrementing top from ~a to ~a"
                name top new-top)

    (pool-set-top! pool new-top)

    (let ([chunk-count (pool-chunk-count pool)])

      (when (= new-top chunk-count)

        (let* ([new-chunk-count  (* chunk-count 2)]
               [chunk-size  (pool-chunk-size  pool)]
               [new-capacity     (* chunk-size new-chunk-count)]
               [new-capacity-    (- new-capacity 1)])

          (pool-debug "~a pool resizing from ~a to ~a"
                      name chunk-count new-chunk-count)

          (table-set! pool "chunk-count" new-chunk-count)

          ; Update core data

          (let ([free-list   (make-free-list   new-chunk-count)]
                [generations (make-generations new-chunk-count)]
                [ref-counts  (make-ref-counts  new-chunk-count)])

            (for ([chunk-index (in-range 0 chunk-count)])

              (pool-debug "~a pool copying core data for chunk ~a"
                          name chunk-index)

              (free-list-set! free-list chunk-index
                              (pool-free       pool chunk-index))

              (generations-set! generations chunk-index
                                (pool-generation pool chunk-index))

              (ref-counts-set! ref-counts chunk-index
                               (pool-ref-count  pool chunk-index)))

            (for ([chunk-index (in-range chunk-count new-chunk-count)])

              (pool-debug "~a pool initializing core data for chunk ~a"
                          name chunk-index)

              (generations-set! generations chunk-index 1)
              (ref-counts-set!  ref-counts  chunk-index 0))

            (table-set! pool "free-list"   free-list)
            (table-set! pool "generations" generations)
            (table-set! pool "ref-counts"  ref-counts))

          ; Update field data

          (let* ([fields      (pool-fields pool)]
                 [field-count (table-length fields 1)]
                 [field-end   (+ field-count 1)])

            (for ([field-index (in-range 1 field-end)])

              (let ([field  (table-ref fields field-index)]
                    [chunks (make-array-table new-capacity-)])

                (for* ([chunk-index (in-range 0 chunk-count)]
                       [chunk-offset (in-range 0 chunk-size)])

                  (let ([value (pool-field pool field
                                           chunk-index chunk-offset)])

                    (pool-debug
                     "~a pool copying ~a from chunk ~a at offset ~a"
                     name field value chunk-index chunk-offset)

                    (table-set! chunks
                                (pool-value-index pool
                                                  chunk-index
                                                  chunk-offset)
                                value)))

                (pool-set-field-chunks! pool field chunks)))))))
    
    new-top))

; Frontend effects

(define (pool-allocate! pool . [: fields array-table])

  (let* ([id          (pool-id   pool)]
         [name        (pool-name pool)]
         [chunk-size  (pool-chunk-size pool)]
         [has-free    (<= 0 (pool-free-top pool))]

         [chunk-index (if has-free
                          (pool-take-free-index! pool)
                          (pool-increment-top! pool))]

         [generation  (pool-generation pool chunk-index)])

    (pool-debug "~a pool allocating chunk ~a at generation ~a"
                (pool-name pool) chunk-index generation)

    (pool-set-ref-count! pool chunk-index 1)

    (let ([field-keys (pool-fields pool)])
      (for ([field-index (in-range 1 (+ (table-length fields) 1))])

        (let ([field-key (table-ref field-keys field-index)]
              [field-val (table-ref fields     field-index)])

          (for ([offset (in-range 0 chunk-size)])

            (pool-debug "~a pool writing ~a into ~a chunk ~a at offset ~a"
                        name field-val field-key chunk-index offset)

            (pool-set-field! pool field-key
                             chunk-index offset
                             field-val)))))

    (handle id chunk-index generation)))

(define (pool-retain! handle)
  (validate-handle handle)
  (let-values ([(id chunk-index generation) (unpack-handle handle)])
    (let ([pool (get-pool id)])

      (pool-debug "~a pool retaining chunk ~a at generation ~a"
                  (pool-name pool) chunk-index generation)

      (pool-increment-ref-count! pool chunk-index)
      handle)))

(define (pool-release! handle)
  (validate-handle handle)
  (let-values ([(id chunk-index generation) (unpack-handle handle)])
    (let ([pool (get-pool id)])

      (pool-debug "~a pool releasing chunk ~a at generation ~a"
                  (pool-name pool) chunk-index generation)

      (let ([ref-count (pool-decrement-ref-count! pool chunk-index)])
        (when (= 0 ref-count)
          (pool-increment-generation! pool chunk-index)
          (pool-return-free-index! pool chunk-index))))))

; Printers

(define (pool-value->string pool chunk-index offset)

  (let ([name   (pool-name pool)]
        [fields (pool-fields pool)])

    (.. (for/fold ([acc (format "#<~a" name)])
                  ([field-index (in-range 1 (+ (table-length fields) 1))])

          (let* ([field (table-ref fields field-index)]
                 [value (pool-field pool field chunk-index offset)])
            
            (.. acc (format " (~a ~a)" field value))))

        ">")))

(define (pool-chunk->string handle)
  (validate-handle handle)
  (let-values ([(id chunk-index _) (unpack-handle handle)])

    (let* ([pool       (get-pool id)]
           [chunk-size (pool-chunk-size pool)])

      (if (= 1 chunk-size)

          (pool-value->string pool chunk-index 0)

          (.. (for/fold ([acc "["])
                        ([offset (in-range 0 chunk-size)])

                (let ([str (pool-value->string pool chunk-index offset)])
                  (.. acc
                      (if (= 0 offset)
                          str
                          (.. " " str)))))

              "]")))))
