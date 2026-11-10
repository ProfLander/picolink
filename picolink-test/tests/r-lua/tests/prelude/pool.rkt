#lang picolink/r-lua

(define-struct gubbin [foo bar baz]
  #:constructor-name %gubbin)

(impl [ToString gubbin:type]
      (define (to-string self)
        (format "#<gubbin ~a ~a ~a>"
                (gubbin-ref self 0)
                (gubbin-ref self 1)
                (gubbin-ref self 2))))

(impl [Update! gubbin:type]
      (define (update! self foo bar baz)
        (gubbin-set! self 0 foo)
        (gubbin-set! self 1 bar)
        (gubbin-set! self 2 baz)))

(impl [Drop gubbin:type]

      (define (drop self)

        (drop (gubbin-ref self 0))
        (gubbin-set! self 0 (void))
        
        (drop (gubbin-ref self 1))
        (gubbin-set! self 1 (void))

        (drop (gubbin-ref self 2))
        (gubbin-set! self 2 (void))))

(define-pool gubbin
  #:constructor [gubbin %gubbin])

(define inst (gubbin 1 2 3))

(print %gubbins-inactive)
(print %gubbins-active)

(update inst 4 5 6)

(print %gubbins-inactive)
(print %gubbins-active)

(drop inst)

(print %gubbins-inactive)
(print %gubbins-active)
