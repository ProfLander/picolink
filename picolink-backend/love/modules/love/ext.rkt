#lang picolink/r-lua/base

(require intrinsic)

(provide (all-defined))

(define-intrinsics
  love
  arg)

(define-intrinsics

  #:template (#%member love _)

  [getVersion           get-version]
  [hasDeprecationOutput has-deprecation-output]
  [isVersionCompatible  is-version-compatible]
  [setDeprecationOutput set-deprecation-output]

  audio
  data
  event
  filesystem
  font
  graphics
  image
  joystick
  keyboard
  math
  mouse
  physics
  sound
  system
  thread
  timer
  touch
  video
  window

  draw
  errorhandler
  load
  lowmemory
  quit
  run
  threaderror
  update

  directorydropped
  displayrotated
  filedropped
  focus
  mousefocus
  resize
  visible

  keypressed
  keyreleased
  textedited
  textinput

  mousemoved
  mousepressed
  mousereleased
  wheelmoved

  gamepadaxis
  gamepadpressed
  gamepadreleased
  joystickadded
  joystickaxis
  joystickhat
  joystickpressed
  joystickreleased
  joystickremoved

  touchmoved
  touchpressed
  touchreleased)
