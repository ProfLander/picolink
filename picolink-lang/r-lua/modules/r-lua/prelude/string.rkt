#lang picolink/r-lua/base

(require (only lua  tostring)

         (only lua/string      sub)
         (only lua/ext/type    string?)
         (only lua/ext/values  foldl-values)

         (only r-lua/prelude/arith             +)
         (only r-lua/prelude/condition         when)
         (only r-lua/prelude/definition        define)
         (only r-lua/prelude/equality          equal?)
         (only r-lua/prelude/error             error)
         (only r-lua/prelude/interface         define-interface)
         (only r-lua/prelude/interface/monoid  mappend)
         (only r-lua/prelude/logic             or)
         (only r-lua/prelude/ordering          <)
         (only r-lua/prelude/s-lua             s-lua))

(provide (all-defined)
         string?)

(define (string-length self)
  (s-lua (#%length self)))

(define (string-empty? self)
  (equal? self ""))

(define (substring self from to)

  (when (< from 0)
    (error "substring start index may not be negative"))

  (when (or (< (string-length self) from)
            (< (string-length self) to))
    (error "substring indices may not be greater than string length"))

  (when (< to from)
    (error
     "substring end index must be less than or equal to start index"))

  (sub self (+ from 1) to))

(define string-append
  (s-lua
   (#%function (#%vararg)
               (#%block
                (#%return
                 (#%call foldl-values mappend #%vararg))))))

(define-interface ToString
  [to-string __tostring
             (λ (a)
               (tostring a))])
