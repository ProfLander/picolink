#lang picolink/r-lua/base

(require (only lua/ext/table  table-length table-ref table-set!)

         (only luajit/ext/table  make-table)

         (only r-lua/prelude/binding     let)
         (only r-lua/prelude/definition  define))

(provide (all-defined))

(define pool-registry (make-table 10 0))

(define (register-pool! pool)
  (let ([id (table-length pool-registry)])
    (table-set! pool "id" id)
    (table-set! pool-registry id pool)))

(define (get-pool id)
  (table-ref pool-registry id))
