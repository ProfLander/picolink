#lang picolink/r-lua/base

(require intrinsic)

(provide (all-defined))

(define-intrinsics
  assert
  [collectgarbage collect-garbage]
  dofile
  error
  _G
  [getfenv        get-fenv]
  [getmetatable   get-metatable]
  ipairs
  load
  [loadfile       load-file]
  [loadstring     load-string]
  next
  pairs
  pcall
  print
  [rawequal       raw-equal]
  [rawget         raw-get]
  [rawset         raw-set]
  select
  [setfenv        set-fenv!]
  [setmetatable   set-metatable!]
  [tonumber       to-number]
  [tostring       to-string]
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
  table

  [newproxy       new-proxy])
