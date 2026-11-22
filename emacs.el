;; -*- lexical-binding: t; -*-

(setq trusted-content :all
      eldoc-echo-area-use-multiline-p nil)

(require 'compile)
(require 'autorevert)
(require 'seq)

(defun compile-current-buffer-default-command ()
  "Run the current buffer's default compilation command without prompting."
  (interactive)
  (compile compile-command))

(global-set-key (kbd "<f4>") #'compile-current-buffer-default-command)

(setq auto-revert-verbose nil
      auto-revert-avoid-polling t
      auto-revert-remote-files t)
(global-auto-revert-mode 1)

(defun racket-config-source-windows (frame)
  "Return non-side windows to the left of FRAME's Racket panels."
  (let* ((layout (frame-parameter frame 'racket-config-window-layout))
         (panel (or (and (window-live-p (nth 1 layout)) (nth 1 layout))
                    (and (window-live-p (nth 2 layout)) (nth 2 layout))))
         (left (and panel (car (window-edges panel)))))
    (when left
      (seq-filter
       (lambda (window)
         (and (not (window-parameter window 'window-side))
              (< (car (window-edges window)) left)))
       (window-list frame 'no-minibuf)))))

(defun racket-config-prevent-window-deletion (window)
  "Prevent deletion of reserved windows and the last source window."
  (let* ((frame (window-frame window))
         (source-windows (racket-config-source-windows frame)))
    (if (and (memq window source-windows)
             (> (length source-windows) 1))
        (let ((ignore-window-parameters t))
          (delete-window window)
          (racket-config-refresh-window-protection frame))
      (message "This is the last source window or a reserved Racket window")
      nil)))

(defun racket-config-refresh-window-protection (frame)
  "Refresh deletion protection for FRAME's Racket layout."
  (let* ((layout (frame-parameter frame 'racket-config-window-layout))
         (source-windows (racket-config-source-windows frame))
         (stored-anchor (car-safe layout))
         (anchor (or (and (memq stored-anchor source-windows) stored-anchor)
                     (car source-windows)))
         (fixed-windows (seq-filter #'window-live-p (cdr layout))))
    (when layout
      (when (and anchor (not (eq stored-anchor anchor)))
        (set-frame-parameter
         frame 'racket-config-window-layout
         (cons anchor (cdr layout))))
      (dolist (window (append source-windows fixed-windows))
        (set-window-parameter
         window 'delete-window #'racket-config-prevent-window-deletion)
        (set-window-parameter
         window 'no-delete-other-windows
         (or (memq window fixed-windows) (eq window anchor)))))))

(add-hook 'window-state-change-functions
          #'racket-config-refresh-window-protection)

(defun racket-config-install-window-layout (frame layout)
  "Register and protect LAYOUT on FRAME."
  (set-frame-parameter frame 'racket-config-window-layout layout)
  ;; Weak dedication lets the display action replace each panel's buffer.
  (set-window-dedicated-p (nth 1 layout) 'racket-config-compilation)
  (set-window-dedicated-p (nth 2 layout) 'racket-config-repl)
  (racket-config-refresh-window-protection frame))

(defun racket-config-window-layout-from-frame (frame)
  "Recover the three principal windows from FRAME."
  (racket-config-refresh-window-protection frame)
  (let* ((layout (frame-parameter frame 'racket-config-window-layout))
         (source-windows (racket-config-source-windows frame)))
    (when (and (memq (car-safe layout) source-windows)
               (window-live-p (nth 1 layout))
               (window-live-p (nth 2 layout)))
      layout)))

(defun racket-config-display-describe-buffer (buffer frame layout)
  "Display BUFFER in the REPL window, temporarily removing the compile window."
  (let ((compile-window (nth 1 layout))
        (repl-window (nth 2 layout)))
    (unless (window-configuration-p
             (frame-parameter
              frame 'racket-config-describe-window-configuration))
      (when (and (window-live-p compile-window)
                 (window-live-p repl-window))
        (set-frame-parameter
         frame 'racket-config-describe-window-configuration
         (current-window-configuration frame))
        (set-window-parameter compile-window 'delete-window nil)
        (delete-window compile-window)))
    (when (window-live-p repl-window)
      (set-window-buffer repl-window buffer)
      repl-window)))

(defun racket-config-display-buffer (buffer _alist)
  "Display Racket-related BUFFER in its reserved window."
  (let* ((frame (selected-frame))
         (layout (frame-parameter frame 'racket-config-window-layout))
         (name (buffer-name buffer)))
    (cond
     ((string-prefix-p "*Racket Describe" name)
      (racket-config-display-describe-buffer buffer frame layout))
     ((or (string-prefix-p "*compilation" name)
          (string-prefix-p "*Racket REPL" name))
      (let ((window (nth (if (string-prefix-p "*compilation" name) 1 2)
                         layout)))
        (when (window-live-p window)
          (set-window-buffer window buffer)
          window))))))

(add-to-list
 'display-buffer-alist
 '("\\`\\*\\(?:Racket Describe\\|compilation\\|Racket REPL\\)"
   (racket-config-display-buffer)))

(defun racket-config-quit-window-around (orig-fun &optional kill window)
  "Restore the Racket layout after quitting a Describe buffer."
  (let* ((window (or window (selected-window)))
         (buffer (window-buffer window))
         (frame (window-frame window))
         (configuration
          (and (with-current-buffer buffer
                 (derived-mode-p 'racket-describe-mode))
               (frame-parameter
                frame 'racket-config-describe-window-configuration))))
    (if (not (window-configuration-p configuration))
        (funcall orig-fun kill window)
      (unwind-protect
          (funcall orig-fun kill window)
        (when (frame-live-p frame)
          (set-frame-parameter
           frame 'racket-config-describe-window-configuration nil)
          (set-window-configuration configuration)
          (let ((layout (racket-config-window-layout-from-frame frame)))
            (when layout
              (racket-config-install-window-layout frame layout))))))))

(unless (advice-member-p #'racket-config-quit-window-around 'quit-window)
  (advice-add 'quit-window :around #'racket-config-quit-window-around))

(defun racket-config-setup-windows ()
  "Set up source, compilation and REPL windows once per frame."
  (let* ((frame (selected-frame))
         (layout (frame-parameter frame 'racket-config-window-layout)))
    (unless (frame-parameter
             frame 'racket-config-describe-window-configuration)
      (if (and (consp layout)
               (window-live-p (nth 1 layout))
               (window-live-p (nth 2 layout))
               (racket-config-source-windows frame))
          (racket-config-refresh-window-protection frame)
        (let* ((source (selected-window))
               (compile-window (split-window-right nil source))
               (repl-window (split-window-below nil compile-window)))
          (set-window-buffer
           compile-window
           (get-buffer-create " *racket-config-compilation-slot*"))
          (set-window-buffer
           repl-window
           (get-buffer-create " *racket-config-repl-slot*"))
          (racket-config-install-window-layout
           frame (list source compile-window repl-window)))))))

(defun racket-config-run (&optional prefix)
  "Fix racket-mode's main submodule handling in compiled modules."
  (interactive "P")
  (racket--repl-run
   (list (racket--buffer-file-name) 'main)
   nil
   (pcase prefix
     (`(4) 'high)
     (`(16) 'debug)
     (_ racket-error-context))))

(defvar racket-config--save-batch-active nil)
(defvar racket-config--save-batch-buffers nil)
(defvar racket-config--suppress-save-some-buffers nil)
(defvar racket-config--active-compile-buffer nil)

(defun racket-config-compile-around (orig-fun &rest args)
  "Record the buffer explicitly being compiled."
  (let ((racket-config--active-compile-buffer (current-buffer)))
    (apply orig-fun args)))

(unless (advice-member-p #'racket-config-compile-around 'compile)
  (advice-add 'compile :around #'racket-config-compile-around))

(defun racket-config-start-compilation (command buffer-name)
  "Run COMMAND in compilation buffer BUFFER-NAME without saving other buffers."
  (let ((compilation-buffer-name-function (lambda (_mode) buffer-name))
        (racket-config--suppress-save-some-buffers t)
        (process (get-buffer-process buffer-name)))
    (when (and process (process-live-p process))
      (delete-process process))
    (compile command)))

(defun racket-config-compile-buffer (buffer)
  "Compile BUFFER without prompting to save any other buffers."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and (derived-mode-p 'racket-mode 'racket-hash-lang-mode)
                 buffer-file-name
                 compile-command)
        (racket-config-start-compilation
         compile-command
         (format "*compilation: %s*" (file-truename buffer-file-name)))))))

(defun racket-config-save-some-buffers-around (orig-fun &rest args)
  "Compile Racket buffers once after the outermost batch save completes."
  (cond
   (racket-config--suppress-save-some-buffers nil)
   (racket-config--save-batch-active (apply orig-fun args))
   (t
    (let ((racket-config--save-batch-active t)
          (racket-config--save-batch-buffers nil))
      (unwind-protect
          (apply orig-fun args)
        (let ((racket-config--save-batch-active nil))
          (mapc #'racket-config-compile-buffer
                (nreverse racket-config--save-batch-buffers))))))))

(unless (advice-member-p
         #'racket-config-save-some-buffers-around 'save-some-buffers)
  (advice-add 'save-some-buffers :around
              #'racket-config-save-some-buffers-around))

(defun racket-config-compile-on-save ()
  "Compile this Racket buffer after saving, batching multi-buffer saves."
  (when (and (not (eq (current-buffer) racket-config--active-compile-buffer))
             buffer-file-name
             compile-command)
    (if racket-config--save-batch-active
        (unless (memq (current-buffer) racket-config--save-batch-buffers)
          (push (current-buffer) racket-config--save-batch-buffers))
      (racket-config-compile-buffer (current-buffer)))))

(defun racket-config-clean-compiled-files ()
  "Delete this Racket file's .zo and .dep artifacts via compilation."
  (interactive)
  (unless (and buffer-file-name
               (derived-mode-p 'racket-mode 'racket-hash-lang-mode))
    (user-error "Current buffer is not a file-visiting Racket buffer"))
  (let* ((file (file-name-nondirectory buffer-file-name))
         (extension (file-name-extension file))
         (base (file-name-sans-extension file))
         (compiled-base (if extension (concat base "_" extension) base))
         (directory
          (expand-file-name "compiled"
                            (file-name-directory buffer-file-name)))
         (command
          (concat "rm -fv -- "
                  (mapconcat
                   #'shell-quote-argument
                   (mapcar (lambda (suffix)
                             (expand-file-name
                              (concat compiled-base "." suffix) directory))
                           '("zo" "dep"))
                   " ")))
         ;; `compile' updates this variable when given a different command.
         (compile-command compile-command))
    (racket-config-start-compilation command "*compilation*")))

(defun racket-config-setup ()
  "Configure compilation and window layout for Racket buffers."
  (when buffer-file-name
    (setq-local compile-command
                (format "raco make %s"
                        (shell-quote-argument buffer-file-name)))
    (local-set-key (kbd "<f3>") #'racket-config-clean-compiled-files)
    (racket-config-setup-windows)
    (add-hook 'after-save-hook #'racket-config-compile-on-save nil t)))

(when (derived-mode-p 'racket-mode 'racket-hash-lang-mode)
  (racket-config-setup))

(with-eval-after-load 'racket-mode
  (unless (advice-member-p #'racket-config-run 'racket-run-and-switch-to-repl)
    (advice-add 'racket-run-and-switch-to-repl :override #'racket-config-run)))
