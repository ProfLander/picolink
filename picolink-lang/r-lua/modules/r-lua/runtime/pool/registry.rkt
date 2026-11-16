#lang picolink/r-lua/base

(require (only lua/ext/ops    +)
         (only lua/ext/table  table-length table-ref table-set!)

         (only luajit/ext/table  make-table)

         (only r-lua/runtime/print       print)

         (only r-lua/prelude/binding     let)
         (only r-lua/prelude/definition  define)
         (only r-lua/prelude/format      format))

(provide (all-defined))

(define pool-registry (make-table 10 0))

(define (register-pool! pool)
  (let ([id (+ (table-length pool-registry) 1)])
    (print (format "registering pool with id ~a" id))
    (table-set! pool-registry id pool)
    (table-set! pool "id" id)))

(define (get-pool id)
  (table-ref pool-registry id))
