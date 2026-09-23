#lang info

(define collection 'multi)

(define deps '("base"
               "picolink-lib"
               "picolink-lang"
               "picolink-doc"
               "picolink-test"))

(define implies '("picolink-lib"
                  "picolink-lang"
                  "picolink-doc"
                  "picolink-test"))

(define pkg-desc "A polyglot programming environment based on Racket.")
(define version "0.0")
(define pkg-authors '(lander))
(define blurb '("Construct compilers and linkers using Racket's module system as a framework."))
(define categories '(utility))

