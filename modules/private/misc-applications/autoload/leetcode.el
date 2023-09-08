;;; private/misc-applications/autoload/leetcode.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +leetcode ()
  (interactive)
  (when (modulep! :ui workspaces)
    (+workspace-switch +leetcode-workspace-name t))
  (leetcode))

;;;###autoload
(defun +leetcode-quit ()
  (interactive)
  (leetcode-quit)
  (when (and (modulep! :ui workspaces)
             (+workspace-exists-p +leetcode-workspace-name))
    (+workspace-delete +leetcode-workspace-name)
    (+workspace/other)))

;;;###autoload
(defun +leetcode-soft-quit ()
  (interactive)
  (if (modulep! :ui workspaces)
      (+workspace/other)
    (quit-window)))
