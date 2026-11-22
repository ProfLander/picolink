;; -*- lexical-binding: t; -*-

(with-eval-after-load 'files
  (setq trusted-content :all)
  (setq eldoc-echo-area-use-multiline-p nil)

  ;; Fix racket-mode's broken main submodule handling in compiled modules
  (defun my-racket-run (&optional prefix)
    (interactive "P")
    (racket--repl-run
     (list (racket--buffer-file-name) 'main)
     nil
     (pcase prefix
       (`(4) 'high)
       (`(16) 'debug)
       (_ racket-error-context))))

  ;; Override default F5 binding
  (advice-add 'racket-run-and-switch-to-repl :override #'my-racket-run)

  ;; Rebuild .rkt files via raco make on save
  (defun my-rebuild-racket ()
    (interactive)
    (when buffer-file-name
      (compile (format "raco make %s"
                       (shell-quote-argument buffer-file-name)))))

  (defun my-rebuild-racket-after-save ()
    (when (and buffer-file-name
               (equal (file-name-extension buffer-file-name) "rkt"))
      (let ((default-directory (file-name-directory buffer-file-name)))
        (my-rebuild-racket))))

  (add-hook 'after-save-hook #'my-rebuild-racket-after-save))
