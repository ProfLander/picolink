#lang picolink/r-lua/base

(require (only lua/table  insert)

         (only lua/ext/table
               array-table
               table-ref
               table-set!
               table-length)

         (only r-lua/prelude/arith                = +)
         (only r-lua/prelude/binding              let*)
         (only r-lua/prelude/condition            when unless)
         (only r-lua/prelude/definition           define)
         (only r-lua/prelude/error                error)
         (only r-lua/prelude/for                  for/fold)
         (only r-lua/prelude/format               format)
         (only r-lua/prelude/if                   if)
         (only r-lua/prelude/interface            impl)
         (only r-lua/prelude/interface/equal      Equal)
         (only r-lua/prelude/interface/length     Length length)
         (only r-lua/prelude/interface/monoid     <>)
         (only r-lua/prelude/interface/order      <)
         (only r-lua/prelude/lambda               λ)
         (only r-lua/prelude/list                 for/list in-list)
         (only r-lua/prelude/print                print)
         (only r-lua/prelude/sequence             sequence-map in-range)
         (only r-lua/prelude/interface/to-string  ToString to-string)
         (only r-lua/prelude/type                 define-type ascribe))

(provide (all-defined))

(define-type vector:type
  #:printed-name vector)

(define (vector . [: vs #%values])
  (ascribe vector:type (array-table vs)))

(define (vector-ref self idx)

  (when (< idx 0)
    (error (format "index ~a out of range for vector ~a" idx self)))

  (unless (< idx (length self))
    (error (format "index ~a out of range for vector ~a" idx self)))
  
  (table-ref self (+ 1 idx)))

(define (vector-set! self idx val)
  (table-set! self (+ 1 idx) val))

(define (vector-push vec val)
  (insert vec val)
  vec)

(impl [Equal vector:type]
      (define (equal? a b)
        (s-lua (#%eq a b))))

(impl [ToString vector:type]
      (define (to-string self)
        (<>
         (for/fold ([acc "#("])
                   ([i (in-range 0 (length self))])
           (let* ([val (vector-ref self i)]
                  [str (to-string val)])
             (if (= i 0)
                 (<> acc str)
                 (<> acc " " str))))
         ")")))

(impl [Length vector:type]
      (define length table-length))

(define (in-vector vec)
  (sequence-map (in-range 0 (length vec))
                (λ (idx)
                  (vector-ref vec idx))))

(define-macro for/vector
  [(_ (for-clause ...) body ...)

   (syntax/loc this-syntax
     (for/fold ([acc (vector)])
               (for-clause ...)
       (vector-push acc (begin body ...))))])

(define (list->vector lst)
  (for/vector ([v (in-list lst)])
    v))

(define (vector->list vec)
  (for/list ([v (in-vector vec)])
    v))
