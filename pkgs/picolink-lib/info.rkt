#lang info

(define collection "picolink")
(define deps '("base" "threading"))
(define build-deps '())
(define clean '("compiled"))
(define compile-omit-paths
  '(#rx"test/.+"
    #rx"lua/modules/.*"))
