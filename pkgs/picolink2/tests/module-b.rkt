#lang picolink2/gaol

(require "module-a.rkt")

bool-false
bool-true
number
string

fst
snd

(fst (snd bool-false bool-true) string)
