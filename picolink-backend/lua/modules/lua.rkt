#lang picolink/r-lua/base

(require intrinsic)

(provide (all-defined))

(define-intrinsics
  assert
  collectgarbage
  dofile
  error
  _G
  getfenv
  getmetatable
  ipairs
  load
  loadfile
  loadstring
  next
  pairs
  pcall
  print
  rawequal
  rawget
  rawset
  select
  setfenv
  setmetatable
  tonumber
  tostring
  type
  unpack
  _VERSION
  xpcall

  require
  module

  coroutine
  debug
  io
  math
  os
  package
  string
  table)
