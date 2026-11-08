#lang picolink/r-lua/base

(require (only lua
               getmetatable
               setmetatable))

(provide (rename getmetatable get-metatable)
         (rename setmetatable set-metatable!))
