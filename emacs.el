;; -*- lexical-binding: t; -*-

(setq trusted-content :all
      eldoc-echo-area-use-multiline-p nil)

(require 'compile)
(require 'autorevert)
(require 'seq)
(require 'desktop)
(require 'frameset)

(setq frameset-filter-alist (copy-tree frameset-filter-alist))
(add-to-list 'frameset-filter-alist
             '(racket-config-window-layout . :never))
(add-to-list 'frameset-filter-alist
             '(racket-config-describe-window-configuration . :never))
(add-to-list 'window-persistent-parameters '(window-side . writable))
(add-to-list 'window-persistent-parameters '(window-slot . writable))
(add-to-list 'window-persistent-parameters
             '(racket-config-window-role . writable))

(defconst racket-config-side-window-width 106)

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
  "Return non-side windows on FRAME."
  (seq-filter
   (lambda (window)
     (not (window-parameter window 'window-side)))
   (window-list frame 'no-minibuf)))

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
         (selected (frame-selected-window frame))
         (anchor (or (and (memq stored-anchor source-windows) stored-anchor)
                     (and (memq selected source-windows) selected)
                     (car source-windows)))
         (fixed-windows (seq-filter #'window-live-p (cdr-safe layout))))
    (when layout
      (when (and anchor (not (eq anchor stored-anchor)))
        (set-frame-parameter
         frame 'racket-config-window-layout
         (cons anchor (cdr layout))))
      (dolist (window (append source-windows fixed-windows))
        (set-window-parameter
         window 'delete-window #'racket-config-prevent-window-deletion)
        (set-window-parameter
         window 'no-delete-other-windows
         (or (memq window fixed-windows) (eq window anchor)))))))

(defun racket-config-window-configuration-change ()
  "Reapply side-window roles and deletion protection after layout changes."
  (dolist (frame (frame-list))
    (let* ((layout (frame-parameter frame 'racket-config-window-layout))
           (compile-window (nth 1 layout))
           (repl-window (nth 2 layout)))
      (when (and (window-live-p compile-window)
                 (window-live-p repl-window))
        (let (window-configuration-change-hook)
          (racket-config-refresh-window-protection frame)
          (set-window-parameter compile-window 'window-side 'right)
          (set-window-parameter compile-window 'window-slot 0)
          (set-window-parameter compile-window 'racket-config-window-role
                                'compilation)
          (set-window-parameter repl-window 'window-side 'right)
          (set-window-parameter repl-window 'window-slot 1)
          (set-window-parameter repl-window 'racket-config-window-role 'repl))))))

(add-hook 'window-configuration-change-hook
          #'racket-config-window-configuration-change)

(defun racket-config-window-with-role (frame role)
  "Return the side window on FRAME assigned to ROLE."
  (let ((role-window
         (seq-find
          (lambda (window)
            (eq (window-parameter window 'racket-config-window-role) role))
          (window-list frame 'no-minibuf)))
        (slot (if (eq role 'compilation) 0 1)))
    (or role-window
        (seq-find
         (lambda (window)
           (and (eq (window-parameter window 'window-side) 'right)
                (equal (or (window-parameter window 'window-slot) 0) slot)))
         (window-list frame 'no-minibuf)))))

(defun racket-config-install-window-layout (frame layout)
  "Register and protect LAYOUT on FRAME."
  (let ((compile-window (nth 1 layout))
        (repl-window (nth 2 layout)))
    (set-frame-parameter frame 'racket-config-window-layout layout)
    (set-window-parameter compile-window 'window-side 'right)
    (set-window-parameter compile-window 'window-slot 0)
    (set-window-parameter compile-window 'racket-config-window-role 'compilation)
    (set-window-parameter repl-window 'window-side 'right)
    (set-window-parameter repl-window 'window-slot 1)
    (set-window-parameter repl-window 'racket-config-window-role 'repl)
    ;; Weak dedication lets the display action replace each panel's buffer.
    (set-window-dedicated-p compile-window 'racket-config-compilation)
    (set-window-dedicated-p repl-window 'racket-config-repl)
    (racket-config-refresh-window-protection frame)))

(defun racket-config-window-layout-from-frame (frame)
  "Recover the source, compilation and REPL windows on FRAME."
  (let* ((stored (frame-parameter frame 'racket-config-window-layout))
         (source-windows (racket-config-source-windows frame))
         (selected (frame-selected-window frame))
         (source
          (or (and (memq (car-safe stored) source-windows) (car stored))
              (and (memq selected source-windows) selected)
              (car source-windows)))
         (compile-window
          (or (and (window-live-p (nth 1 stored)) (nth 1 stored))
              (racket-config-window-with-role frame 'compilation)))
         (repl-window
          (or (and (window-live-p (nth 2 stored)) (nth 2 stored))
              (racket-config-window-with-role frame 'repl))))
    (when (and source compile-window repl-window)
      (let ((layout (list source compile-window repl-window)))
        (racket-config-install-window-layout frame layout)
        layout))))

(defun racket-config-placeholder-buffer (role)
  "Return the hidden placeholder buffer for panel ROLE."
  (let ((buffer
         (get-buffer-create
          (pcase role
            ('compilation " *racket-config-compilation-slot*")
            ('repl " *racket-config-repl-slot*")))))
    (with-current-buffer buffer
      (setq-local desktop-save-buffer nil))
    buffer))

(defun racket-config-create-side-window (frame role)
  "Create the right-side window for panel ROLE on FRAME."
  (let* ((slot (if (eq role 'compilation) 0 1))
         (buffer (racket-config-placeholder-buffer role))
         (window
          (with-selected-frame frame
            (display-buffer
             buffer
             `((display-buffer-in-side-window)
               (side . right)
               (slot . ,slot)
               (window-width . ,racket-config-side-window-width)
               (dedicated . nil))))))
    (when window
      (set-window-parameter window 'racket-config-window-role role))
    window))

(defun racket-config-ensure-window-layout (frame)
  "Ensure FRAME has its source area and both Racket side windows."
  (let* ((compile-window
          (or (racket-config-window-with-role frame 'compilation)
              (racket-config-create-side-window frame 'compilation)))
         (repl-window
          (or (racket-config-window-with-role frame 'repl)
              (racket-config-create-side-window frame 'repl)))
         (source-windows (racket-config-source-windows frame))
         (selected (frame-selected-window frame))
         (source (or (and (memq selected source-windows) selected)
                     (car source-windows))))
    (when (and source compile-window repl-window)
      (let ((layout (list source compile-window repl-window)))
        (racket-config-install-window-layout frame layout)
        layout))))

(defun racket-config-reset-panel-buffers (layout)
  "Replace restored file buffers in LAYOUT's panels with placeholders."
  (set-window-buffer (nth 1 layout)
                     (racket-config-placeholder-buffer 'compilation))
  (set-window-buffer (nth 2 layout)
                     (racket-config-placeholder-buffer 'repl)))

(defun racket-config-desktop-prepare-side-windows ()
  "Create placeholder buffers before Desktop restores the frameset."
  (racket-config-placeholder-buffer 'compilation)
  (racket-config-placeholder-buffer 'repl))

(add-hook 'desktop-delay-hook
          #'racket-config-desktop-prepare-side-windows)

(defvar racket-config--desktop-saving nil)

(defun racket-config-desktop-save-frameset-around (orig-fun &rest args)
  "Substitute persistent placeholders while Desktop captures the frameset."
  (if (or racket-config--desktop-saving
          (not desktop-restore-frames))
      (apply orig-fun args)
    (let ((racket-config--desktop-saving t)
          saved-windows)
      (unwind-protect
          (progn
            (racket-config-desktop-prepare-side-windows)
            (dolist (frame (frame-list))
              (let ((layout (racket-config-window-layout-from-frame frame)))
                (when layout
                  (dolist (spec `((,(nth 1 layout) . compilation)
                                  (,(nth 2 layout) . repl)))
                    (let* ((window (car spec))
                           (placeholder
                            (racket-config-placeholder-buffer (cdr spec)))
                           (buffer (window-buffer window)))
                      (unless (eq buffer placeholder)
                        (push (list window buffer
                                    (window-point window)
                                    (window-start window)
                                    (window-dedicated-p window))
                              saved-windows)
                        (set-window-dedicated-p window nil)
                        (set-window-buffer window placeholder))))))
            (apply orig-fun args))
        (dolist (entry saved-windows)
          (pcase-let ((`(,window ,buffer ,point ,start ,dedicated) entry))
            (when (and (window-live-p window) (buffer-live-p buffer))
              (set-window-dedicated-p window nil)
              (set-window-buffer window buffer)
              (set-window-point window point)
              (set-window-start window start t)
              (set-window-dedicated-p window dedicated)))))))))

(unless (advice-member-p
         #'racket-config-desktop-save-frameset-around 'desktop-save-frameset)
  (advice-add 'desktop-save-frameset :around
              #'racket-config-desktop-save-frameset-around))

(defun racket-config-frame-has-racket-buffer-p (frame)
  "Return non-nil when FRAME displays a Racket source or REPL buffer."
  (seq-some
   (lambda (window)
     (with-current-buffer (window-buffer window)
       (derived-mode-p 'racket-mode 'racket-hash-lang-mode 'racket-repl-mode)))
   (window-list frame 'no-minibuf)))

(defun racket-config-restore-desktop-layouts ()
  "Reconstruct Racket side-window state after Desktop restores frames."
  (add-hook 'desktop-delay-hook
            #'racket-config-desktop-prepare-side-windows)
  (dolist (frame (frame-list))
    (when (or (racket-config-window-with-role frame 'compilation)
              (racket-config-window-with-role frame 'repl)
              (racket-config-frame-has-racket-buffer-p frame))
      (let ((layout (racket-config-ensure-window-layout frame)))
        (when layout
          (racket-config-reset-panel-buffers layout)
          (racket-config-install-window-layout frame layout))))))

(add-hook 'desktop-after-read-hook
          #'racket-config-restore-desktop-layouts)

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
         (layout (racket-config-window-layout-from-frame frame))
         (name (buffer-name buffer)))
    (cond
     ((string-prefix-p "*Racket Describe" name)
      (and layout
           (racket-config-display-describe-buffer buffer frame layout)))
     ((or (string-prefix-p "*compilation" name)
          (string-prefix-p "*Racket REPL" name))
      (when layout
        (let ((window (nth (if (string-prefix-p "*compilation" name) 1 2)
                           layout)))
          (when (window-live-p window)
            (set-window-buffer window buffer)
            window)))))))

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
  "Set up the source area and two right-side Racket panel windows."
  (let* ((frame (selected-frame))
         (describe-configuration
          (frame-parameter
           frame 'racket-config-describe-window-configuration)))
    (racket-config-desktop-prepare-side-windows)
    (unless (or (window-configuration-p describe-configuration)
                (desktop-restoring-frameset-p))
      (unless (racket-config-window-layout-from-frame frame)
        (racket-config-ensure-window-layout frame)))))

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
