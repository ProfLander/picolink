#lang picolink/r-lua/base

(require (only lua         error)
         (only lua/math    floor)
         (only lua/string  format)

         (only lua/ext/ops   = + - * / ^ % < <=)
         (only lua/ext/type  number?)

         (only r-lua/prelude/binding     let let* let-values)
         (only r-lua/prelude/condition   when unless)
         (only r-lua/prelude/definition  define)
         (only r-lua/prelude/logic       and)
         (only r-lua/prelude/values      values))

(provide handle
         handle?
         handle-pool
         handle-chunk
         handle-generation
         unpack-handle
         handle->string)

; Bit packing

(define handle-pool-bits        17)
(define handle-chunk-bits       18)
(define handle-generation-bits  17)

(unless (and (< 0 handle-pool-bits)
             (= handle-pool-bits
                (floor handle-pool-bits))

             (< 0 handle-chunk-bits)
             (= handle-chunk-bits
                (floor handle-chunk-bits))
             
             (< 0 handle-generation-bits)
             (= handle-generation-bits
                (floor handle-generation-bits))

             (<= (+ handle-pool-bits
                    (+ handle-chunk-bits
                       handle-generation-bits))
                 52))

  (error "invalid handle bit counts"))

(define handle-pool-limit       (^ 2 handle-pool-bits))
(define handle-chunk-limit      (^ 2 handle-chunk-bits))
(define handle-generation-limit (^ 2 handle-generation-bits))

(define handle-pool-shift       (* handle-chunk-limit
                                   handle-generation-limit))

(define handle-chunk-shift      handle-generation-limit)

(define handle-marker           (^ 2 52))
(define handle-limit            (* handle-pool-limit
                                   handle-pool-shift))

(define (uint? n limit)

  (and (number? n limit)
       (<= 0    n)
       (<  n    limit)
       (=  n    (floor n))))

(define (handle-pool? n)
  (uint? n handle-pool-limit))

(define (handle-chunk? n)
  (uint? n handle-chunk-limit))

(define (handle-generation? n)
  (uint? n handle-generation-limit))

(define (handle? n)

  (and (number?          n)
       (<= handle-marker n)
       (<  n             (+ handle-marker handle-limit))
       (=  n             (floor n))))

(define (check-handle-pool n)
  (unless (handle-pool? n)
    (error (format "%s is not a valid handle ID" n))))

(define (check-handle-chunk n)
  (unless (handle-chunk? n)
    (error (format "%s is not a valid handle chunk index" n))))

(define (check-handle-generation  n)
  (unless (handle-generation? n)
    (error (format "%s is not a valid handle generation" n))))

(define (check-handle n)
  (unless (handle? n)
    (error (format "%s is not a valid handle" n))))

; Constructor

(define (handle pool chunk generation)

  (check-handle-pool       pool)
  (check-handle-chunk      chunk)
  (check-handle-generation generation)
  
  (+ handle-marker
     (+ (* pool handle-pool-shift)
        (+ (* chunk handle-chunk-shift)
           generation))))

; Getters

(define (handle-pool handle)
  (check-handle handle)

  (let* ([offset  (- handle handle-marker)]
         [shifted (/ offset handle-pool-shift)])

    (floor shifted)))

(define (handle-chunk handle)
  (check-handle handle)

  (let* ([offset  (- handle handle-marker)]
         [shifted (/ offset handle-chunk-shift)]
         [floored (floor shifted)])

    (% floored handle-chunk-limit)))

(define (handle-generation handle)
  (check-handle handle)

  (let ([offset (- handle handle-marker)])
    (% offset handle-generation-limit)))

(define (unpack-handle handle)
  (values (handle-pool       handle)
          (handle-chunk      handle)
          (handle-generation handle)))

; Printer

(define (handle->string handle)
  (let-values ([(pool chunk generation) (unpack-handle handle)])
    (format "#handle<(pool %s) (chunk %s) (generation %s)>"
            pool chunk generation)))
