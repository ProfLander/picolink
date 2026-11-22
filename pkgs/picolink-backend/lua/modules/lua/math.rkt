#lang picolink/r-lua/base

(require (only intrinsic  define-intrinsics))

(provide (all-defined))

(define-intrinsics
  #:template (#%member (#%member _G math) _)
  abs
  acos
  asin
  atan
  atan2
  ceil
  cos
  cosh
  deg
  exp
  floor
  fmod
  frexp
  huge
  ldexp
  log
  log10
  max
  min
  modf
  pi
  pow
  rad
  random
  [randomseed  random-seed]
  sin
  sinh
  sqrt
  tan
  tanh)
