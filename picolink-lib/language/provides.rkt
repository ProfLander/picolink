#lang racket/base

(require racket/contract
         racket/list
         racket/set
         racket/sequence

         picolink/binding)

(provide (all-defined-out))

(struct module-provides (inner)
  #:transparent)

(define/contract (make-module-provides [provides null])
  (->* []
       [(listof (or/c identifier? binding?))]
       module-provides?)

  (let* ([provides (map lift-binding provides)]
         [duplicate (check-duplicates provides)])

    (when duplicate
      (raise-syntax-error
       #f
       "duplicate provide:"
       (binding-ident duplicate)))

    (module-provides (list->set provides))))

(define/contract (module-provides-member? provides binding)
  (-> module-provides? binding? boolean?)
  (set-member? (module-provides-inner provides) binding))

(define/contract (module-provides-map provides f)
  (-> module-provides? (-> binding? any/c) (listof any/c))
  (set-map (module-provides-inner provides) f))

(define/contract (in-module-provides provides)
  (-> module-provides? (sequence/c binding?))
  (in-set (module-provides-inner provides)))
