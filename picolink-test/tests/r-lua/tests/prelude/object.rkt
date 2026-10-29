#lang picolink/r-lua

(require (only lua
               getmetatable
               setmetatable
               tostring
               error)

         (only lua/string
               format)

         (only lua/ext/table
               table-ref
               table-set!)

         (only lua/ext/type
               nil?)

         (only lua/ext/nil
               nil))

(define-object actor [name position])

(define (print-actor self)
  (print (actor-name self))
  (print (actor-position self)))

(define (print-actor-handle handle)
  (print (format "%s:" handle))
  (print-actor handle)
  (print ""))

(define (print-actor-pool)
  (print "pool:")
  (print (format "inactive: %s" %actors-inactive))
  (print (format "  active: %s" %actors-active))
  (print ""))

(define actor-1 (actor "chap" "a"))
(print-actor-handle actor-1)
(print-actor-pool)

(define actor-2 (actor-update actor-1 nil "b"))
(print-actor-handle actor-2)
(print-actor-pool)

(actor-release actor-1)
(print-actor-pool)

(define actor-3 (actor-update actor-2 "bloke" nil))
(print-actor-handle actor-3)
(print-actor-pool)

(actor-release actor-2)
(print-actor-pool)

(define actor-4 (actor-update actor-3 "fella" "c"))
(print-actor-handle actor-4)
(print-actor-pool)

(actor-release actor-3)
(print-actor-pool)
