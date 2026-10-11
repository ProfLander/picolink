#lang info

(define collection "picolink")
(define deps '("base" "picolink-lib"))
(define build-deps '())
(define clean '("compiled"))
(define compile-omit-paths
  '(#rx".+/modules/.+"))
