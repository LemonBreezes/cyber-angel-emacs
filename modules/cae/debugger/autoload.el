;;; private/debugger/autoload.el -*- lexical-binding: t; -*-

(eval-when-compile (require 'hydra nil t))

;;;###autoload
(defun cae-debugger-dap-kill-all-sessions-and-restart ()
  (interactive)
  (dolist (buf (doom-visible-buffers))
    (if (string-match-p (format "^\\*compilation\\*<%s>"
                                (doom-project-name))
                        (buffer-name buf))
        (kill-buffer buf)))
  (when (modulep! :ui popup)
    (+popup/close-all))
  (unwind-protect (dap-delete-all-sessions)
    (when-let ((workspace-project (cl-find (+workspace-current-name)
                                           (projectile-relevant-known-projects)
                                           :test #'string-match-p)))
      (unless (string= (projectile-project-name)
                       (+workspace-current-name))
        (projectile-switch-project-by-name workspace-project)))
    (dap-debug-last)))

;;;###autoload
(defun cae-debugger-quit-or-delete-or-send-eof (arg)
  (interactive "p")
  (if (and (eobp)
           (looking-back (concat dap-ui-repl-prompt "\\s-*") nil))
      (kill-buffer)
    (comint-delchar-or-maybe-eof arg)))

;;;###autoload
(defun cae-debugger-open-repl ()
  (interactive)
  (let ((repl-buffer
         (or (get-buffer "*dap-ui-repl*")
             (progn (dap-ui-repl)
                    (let ((buf (get-buffer "*dap-ui-repl*")))
                      (bury-buffer buf)
                      buf)))))
    (with-current-buffer repl-buffer
      (doom-mark-buffer-as-real-h))
    (pop-to-buffer repl-buffer)))

;;;###autoload
(defun cae-debugger-run-or-pop-to-gdb ()
  (interactive)
  (require 'gud)
  (cond ((and gud-comint-buffer
              (get-buffer-window gud-comint-buffer))
         (delete-window (get-buffer-window gud-comint-buffer)))
        ((buffer-live-p gud-comint-buffer)
         (call-interactively #'gdb-display-gdb-buffer))
        (t (call-interactively #'gdb))))
