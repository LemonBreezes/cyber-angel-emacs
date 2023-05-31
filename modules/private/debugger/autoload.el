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
  (cae-debugger-dap-quit-session-a nil)
  (dap-delete-all-sessions)
  (when-let ((workspace-project (cl-find (+workspace-current-name)
                                         (projectile-relevant-known-projects)
                                         :test #'string-match-p)))
    (unless (string= (projectile-project-name)
                     (+workspace-current-name))
      (projectile-switch-project-by-name workspace-project)))
  (dap-debug-last))

;;;###autoload
(defun cae-debugger-gud-comint-send-input ()
  (interactive)
  ;; Not sure why `M-x gdb' binds this command everywhere, but it does.
  (cond ((derived-mode-p 'gdb-frames-mode)
         (call-interactively #'gdb-select-frame))
        (t
         (call-interactively #'comint-send-input))))

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

(defvar cae-debugger--session-workspace-map ()
  "Alist of (session . workspace) pairs.")

;;;###autoload
(defun cae-debugger-mark-session-h (session)
  "Mark the current session with the current workspace."
  (setf (alist-get session cae-debugger--session-workspace-map
                   nil nil #'equal)
        (+workspace-current-name)))

;;;###autoload
(defun cae-debugger-dap-switch-to-workspace-h ()
  "Switch to the workspace associated with the current session."
  (when-let ((session (dap--cur-session))
             (workspace (alist-get session cae-debugger--session-workspace-map
                                   nil nil #'equal)))
    (unless (string= workspace (+workspace-current-name))
      (+workspace-switch workspace))))
