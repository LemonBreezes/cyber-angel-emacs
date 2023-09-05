;;; private/gnus/autoload/gnus.el -*- lexical-binding: t; -*-

(defvar +gnus-workspace-name "*gnus*"
  "Name of the workspace created by `=gnus', dedicated to mu4e.")
(defvar +gnus--old-wconf nil)

;;;###autoload
(defun =gnus ()
  (interactive)
  (if (modulep! :ui workspaces)
      ;; delete current workspace if empty
      ;; this is useful when mu4e is in the daemon
      ;; as otherwise you can accumulate empty workspaces
      (progn
        (unless (+workspace-buffer-list)
          (+workspace-delete (+workspace-current-name)))
        (+workspace-switch +gnus-workspace-name t))
    (setq +gnus--old-wconf (current-window-configuration))
    (let ((ignore-window-parameters t))
      (delete-other-windows))
    (switch-to-buffer (doom-fallback-buffer)))
  (call-interactively #'gnus-unplugged))

(add-hook 'gnus-exit-gnus-hook
          (cae-defun +gnus-kill-workspace-h ()
            (when (featurep 'bbdb)
              (bbdb-save))
            (when (and (modulep! :ui workspaces)
                       (+workspace-exists-p +gnus-workspace-name))
              (+workspace/delete +gnus-workspace-name))))
