;;; private/debugger/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-debugger-dap-quit-session-a (_)
  (ignore-errors
    (let ((ignore-window-parameters t))
      (cl-loop for buf being the buffers
               when (string-match-p "gdb" (buffer-name buf)) do
               (cae-hacks-always-yes-a #'doom-kill-buffer-and-windows buf)))))

;;;###autoload
(defun cae-debugger-dap-kill-all-sessions-and-restart ()
  (interactive)
  (dap-delete-all-sessions)
  (cae-debugger-quit-session-a nil)
  (when-let ((workspace-project (cl-find (+workspace-current-name)
                                         (projectile-relevant-known-projects)
                                         :test #'string-match-p)))
    (unless (string= (projectile-project-name)
                     (+workspace-current-name))
      (projectile-switch-project-by-name workspace-project)))
  (dap-debug-last))

;;;###autoload
(defun cae-debugger-gdb-select-frame ()
  (interactive)
  (if (derived-mode-p 'gdb-frames-mode)
      (call-interactively #'gdb-select-frame)
    (call-interactively #'comint-send-input)))

(defun cae-debugger--which-key-inhibit-hook ()
  (setq which-key-inhibit nil)
  (remove-hook 'pre-command-hook
               #'cae-debugger--which-key-inhibit-hook))

(defvar cae-debugger--global-map (make-sparse-keymap))

;;;###autoload
(defun cae-debugger-lazy-load-gud ()
  (interactive)
  (require 'gud)
  (require 'edebug)
  (setq unread-command-events (list ?\C-x ?\C-a))
  (setq which-key-inhibit t)
  ;; (add-hook 'pre-command-hook #'cae-debugger--which-key-inhibit-hook)
  (run-with-idle-timer
   which-key-idle-delay nil
   (lambda ()
     (when which-key-inhibit
       (which-key-show-keymap
        (prog1 'cae-debugger--global-map
          (setq cae-debugger--global-map
                (cons 'keymap
                      (let* ((alist1 (cdr-safe (lookup-key (current-global-map) (kbd "C-x C-a"))))
                             (alist2 (cdr-safe (lookup-key (current-local-map) (kbd "C-x C-a"))))
                             (keys (mapcar #'car alist2)))
                        (dolist (key keys)
                          (setq alist1 (assq-delete-all key alist1)))
                        (append alist1 alist2))))))))))
