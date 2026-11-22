#lang picolink/s-lua

(#%local [foo] [1234])
(#%call print (#%add foo foo))
