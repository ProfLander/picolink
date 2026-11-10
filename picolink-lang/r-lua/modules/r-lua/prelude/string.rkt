#lang picolink/r-lua/base

(require (only lua/string      sub)
         (only lua/ext/type    string?)
         (only lua/ext/values  foldl-values)

         (only r-lua/prelude/arith                +)
         (only r-lua/prelude/condition            when)
         (only r-lua/prelude/definition           define)
         (only r-lua/prelude/function             identity)
         (only r-lua/prelude/error                error)
         (only r-lua/prelude/interface            impl)
         (only r-lua/prelude/interface/monoid     mappend)
         (only r-lua/prelude/interface/to-string  ToString)
         (only r-lua/prelude/interface/order      <)
         (only r-lua/prelude/interface/copy       Copy)
         (only r-lua/prelude/interface/drop       Drop)
         (only r-lua/prelude/interface/equal      Equal equal?)
         (only r-lua/prelude/interface/length     Length length)
         (only r-lua/prelude/interface/monoid     Monoid)
         (only r-lua/prelude/logic                or)
         (only r-lua/prelude/print                print)
         (only r-lua/prelude/s-lua                s-lua)
         (only r-lua/prelude/type                 String)
         (only r-lua/prelude/void                 void))

(provide (all-defined)
         string?)

(impl [Copy String]
      (define copy identity))

(impl [Drop String]
      (define drop void))

(impl [Equal String]
      (define (equal? a b)
        (s-lua (#%eq a b))))

(impl [Length String]
      (define (length self)
        (s-lua (#%length self))))

(impl [Monoid String]
      (define (mappend a b)
        (s-lua (#%cat a b))))

(impl [ToString String]
      (define to-string identity))

(define (string-empty? self)
  (equal? self ""))

(define (substring self from to)

  (when (< from 0)
    (error "substring start index may not be negative"))

  (when (or (< (length self) from)
            (< (length self) to))
    (error "substring indices may not be greater than string length"))

  (when (< to from)
    (error
     "substring end index must be less than or equal to start index"))

  (sub self (+ from 1) to))
