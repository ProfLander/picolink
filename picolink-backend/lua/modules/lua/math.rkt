#lang picolink/r-lua/base

(require intrinsic)

(provide (all-defined))

(define-intrinsics
  #:template (#%member math _)
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
  randomseed
  sin
  sinh
  sqrt
  tan
  tanh)
