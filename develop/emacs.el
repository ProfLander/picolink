;; -*- lexical-binding: t; -*-

(evil-define-key 'normal racket-repl-mode-map
  (kbd "<up>") #'racket-repl-previous-input)

(evil-define-key 'normal racket-repl-mode-map
  (kbd "<down>") #'racket-repl-next-input)

(evil-define-key 'normal racket-repl-mode-map
  (kbd "<left>") #'racket-repl-previous-prompt-or-run)

(evil-define-key 'normal racket-repl-mode-map
  (kbd "<right>") #'racket-repl-next-prompt-or-run)

(evil-define-key 'normal racket-repl-mode-map
  (kbd "RET") #'racket-repl-submit)
