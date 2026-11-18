#lang picolink/r-lua/base

(require (only intrinsic  define-intrinsics))

(provide (all-defined))

(define-intrinsics

  #:template (#%member (#%member (#%member _G love) filesystem) _)

  FileDecoder
  FileMode
  FileType

  append
  [areSymlinksEnabled     are-symlinks-enabled]
  [createDirectory        create-directory]
  [getAppdataDirectory    get-appdata-directory]
  [getCRequirePath        get-c-require-path]
  [getDirectoryItems      get-directory-items]
  [getIdentity            get-identity]
  [getInfo                get-info]
  [getRealDirectory       get-real-directory]
  [getRequirePath         get-require-path]
  [getSaveDirectory       get-save-directory]
  [getSource              get-source]
  [getSourceBaseDirectory get-source-base-directory]
  [getUserDirectory       get-user-directory]
  [getWorkingDirectory    get-working-directory]
  init
  [isFused                fused?]
  lines
  load
  mount
  [newFile                new-file]
  [newFileData            new-file-data]
  read
  remove
  [setCRequirePath        set-c-require-path!]
  [setIdentity            set-identity!]
  [setRequirePath         set-require-path!]
  [setSource              set-source!]
  [setSymlinksEnabled     set-symlinks-enabled!]
  unmount
  write)
