#lang picolink/r-lua

(require (only lua/ext/nil  nil))

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

(print "actor-1:")
(define actor-1 (actor "chap" "a"))
(print-actor-handle actor-1)
(print-actor-pool)

(print "actor-2:")
(define actor-2 (update actor-1 (actor-name actor-1) "b"))
(print-actor-handle actor-2)
(print-actor-pool)

(print "dropping actor-1...")
(drop actor-1)
(print-actor-pool)

(print "actor-3:")
(define actor-3 (update actor-2 "bloke" (actor-position actor-2)))
(print-actor-handle actor-3)
(print-actor-pool)

(print "dropping actor-2...")
(drop actor-2)
(print-actor-pool)

(print "actor-4:")
(define actor-4 (update actor-3 "fella" "c"))
(print-actor-handle actor-4)
(print-actor-pool)

(print "dropping actor-3...")
(drop actor-3)
(print-actor-pool)

(print "dropping actor-4...")
(drop actor-4)
(print-actor-pool)
