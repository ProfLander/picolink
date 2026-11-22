#lang picolink/r-lua/base

(require (only lua  %require)

         (only r-lua/prelude/definition  define))

(provide (all-defined))

(define new   (%require "table.new"))
(define clear (%require "table.clear"))
