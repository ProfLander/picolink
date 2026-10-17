#lang picolink/r-lua

(require (only-in lua
                  print))

(provide string-append)

(define %cat
  (λ (a b)
    (#%intrinsic (#%cat a b))))

(define-macro string-append
  [(_ a b rest ...+)
   #'(let ([cat (string-append a b)])
       (string-append cat rest ...))]
  [(_ a b)
   #'(%cat a b)])

(print "string-append:")
(string-append "foo" "bar" "baz")
