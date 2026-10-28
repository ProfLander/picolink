#lang picolink/r-lua/base

(require (only prelude/equality
               eq?)

         (only lua
               print)

         (only lua/ext/values
               foldl-values)

         prelude/surface)

(provide (all-defined))

(define = eq?)

(define (< a b)
  (s-lua (#%lt a b)))

(define (> a b)
  (s-lua (#%gt a b)))

(define (<= a b)
  (s-lua (#%le a b)))

(define (>= a b)
  (s-lua (#%ge a b)))

(define (%add a b)
  (s-lua (#%add a b)))

(define +
  (s-lua
   (#%function (#%vararg)
               (#%block
                (#%return
                 (#%call foldl-values %add #%vararg))))))

(define (%sub a b)
  (s-lua (#%sub a b)))

(define -
  (s-lua
   (#%function (#%vararg)
               (#%block
                (#%if (#%eq (#%call select "#" #%vararg) 1)

                      (#%then
                       (#%block
                        (#%return
                         (#%neg #%vararg))))

                      (#%else
                       (#%block
                        (#%return
                         (#%call foldl-values %sub #%vararg)))))))))

(define (%mul a b)
  (s-lua (#%mul a b)))

(define *
  (s-lua
   (#%function (#%vararg)
               (#%block
                (#%return
                 (#%call foldl-values %mul #%vararg))))))

(define (%div a b)
  (s-lua (#%div a b)))

(define /
  (s-lua
   (#%function (#%vararg)
               (#%block
                (#%return
                 (#%call foldl-values %div #%vararg))))))

(define (expt a b)
  (s-lua (#%exp a b)))

(define (modulo a b)
  (s-lua (#%mod a b)))
