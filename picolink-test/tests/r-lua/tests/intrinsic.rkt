#lang picolink/r-lua

(require (in lua setmetatable)
         (in lua/string format))

; FIXME: Function return values do not hoist

#;(define cons-index
    (λ (key)
      (if (#%intrinsic (#%eq key "__tag"))
          "pair"
          (void))))

; TODO: Allow nested use of define-macro
;       (i.e. Allow defining non-expression macros)

(define-macro +
  [(_ a b rest ...+)
   #'(let ([+rest (+ b rest ...)])
       (#%intrinsic (#%add a +rest)))]
  [(_ a b)
   #'(#%intrinsic (#%add a b))])

(+ 1 2 3 4)

; FIXME: Incorrect associativity for binop case

(define-macro -
  [(_ a b rest ...+)
   #'(let ([-rest (- b rest ...)])
       (#%intrinsic (#%sub a -rest)))]

  [(_ a b)
   #'(#%intrinsic (#%sub a b))]

  [(_ a)
   #'(#%intrinsic (#%neg a))])

(- 10 4 2 1)

(- 10)

(define *
  (λ (a b)
    (#%intrinsic (#%mul a b))))

(define /
  (λ (a b)
    (#%intrinsic (#%div a b))))

(define exp
  (λ (a b)
    (#%intrinsic (#%exp a b))))

(define modulo
  (λ (a b)
    (#%intrinsic (#%mod a b))))

(define string-append
  (λ (a b)
    (#%intrinsic (#%cat a b))))

(define <
  (λ (a b)
    (#%intrinsic (#%lt a b))))

(define >
  (λ (a b)
    (#%intrinsic (#%gt a b))))

(define <=
  (λ (a b)
    (#%intrinsic (#%le a b))))

(define >=
  (λ (a b)
    (#%intrinsic (#%ge a b))))

(define eq?
  (λ (a b)
    (#%intrinsic (#%eq a b))))

(define and
  (λ (a b)
    (#%intrinsic (#%and a b))))

(define or
  (λ (a b)
    (#%intrinsic (#%or a b))))

(define not
  (λ (v)
    (#%intrinsic (#%not v))))

(define-macro vector
  [(_ v ...)
   (with-syntax ([(k ...) (map (λ (_) (generate-temporary))
                               (attribute v))])
     #'(let ([k v]
             ...)
         (#%intrinsic (#%table v ...))))])

(define-macro hash
  [(_ (~seq k v) ...)
   (with-syntax ([(k-str ...) (map (compose symbol->string
                                            syntax-e)
                                   (attribute k))]
                 [(v-key ...) (map (λ (_) (generate-temporary))
                                   (attribute v))])
     #'(let ([k k-str]
             ...
             [v-key v]
             ...)
         (#%intrinsic
          (#%table [k v] ...))))])

(define null-to-string
  (λ (self)
    "()"))

(define null
  (setmetatable (vector)
                (#%intrinsic (#%table [__tostring null-to-string]))

                ; FIXME: Local definition RHS does not lift
                #;(hash __tostring null-to-string)))

(define tag
  (λ (v)
    (#%intrinsic (#%member v __tag))))

(define pair?
  (λ (v)
    (eq? (tag v) "pair")))

(define car
  (λ (v)
    (#%intrinsic (#%member v 1))))

(define cdr
  (λ (v)
    (#%intrinsic (#%member v 2))))

(define cadr
  (λ (v)
    (car (cdr v))))

(define cddr
  (λ (v)
    (cdr (cdr v))))

(define cons-to-string
  (λ (self)
    (format "(%s . %s)" (car self) (cdr self))))

(define cons
  (λ (a b)

    (let ([cons-index
           (#%intrinsic
            (#%function
             (self key)
             (#%block
              (#%if (#%eq key "__tag")
                    (#%then
                     (#%block
                      (#%return "pair")))))))])

      (setmetatable (vector a b)
                    (hash __index    cons-index
                          __tostring cons-to-string)))))

(define cell (cons 1 (cons 2 null)))
(pair? cell)

cell

(car cell)

(cdr cell)

(cadr cell)

(cddr cell)
