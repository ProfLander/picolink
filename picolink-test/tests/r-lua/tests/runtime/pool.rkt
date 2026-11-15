#lang picolink/r-lua

(require (only lua/ext/table  table-length table-ref)

         r-lua/runtime/pool
         r-lua/runtime/pool/handle
         r-lua/runtime/pool/registry)

(define pool (make-pool "test" 1 2
                        "x" "y"))
""

(define inst-1  (pool-allocate! pool 2 3))
"inst-1:"
inst-1
""

(define inst-2  (pool-allocate! pool 4 5))
"inst-2:"
inst-2
""

; automatic resize
(define inst-3  (pool-allocate! pool inst-1 inst-2))
"inst-3:"
inst-3
""

(copy inst-2)
""

(drop inst-3)
""

"inst-2:"
inst-2
""

(define inst-4  (pool-allocate! pool 8 9))
inst-4
""

(define inst-5  (pool-allocate! pool 10 11))
inst-5
""

(define inst-6  (pool-allocate! pool 12 13))
inst-6
""

(define inst-7  (pool-allocate! pool 14 15))
inst-7
""

; automatic resize
(define inst-8  (pool-allocate! pool 16 17))
inst-8
""

(define inst-9  (pool-allocate! pool 18 19))
inst-9
""

(define inst-10 (pool-allocate! pool 20 21))
inst-10
""

; post-allocation sanity check

inst-2
""

inst-4
""

inst-5
""

inst-6
""

inst-7
""

inst-8
""

inst-9
""

inst-10

