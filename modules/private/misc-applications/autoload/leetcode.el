;;; private/misc-applications/autoload/leetcode.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +leetcode ()
  (interactive)
  (when (modulep! :ui workspaces)
    (+workspace-switch "*leetcode*" t))
  (leetcode))

;;;###autoload
(defun +leetcode-quit ()
  (interactive)
  (leetcode-quit)
  (when (and (modulep! :ui workspaces)
             (+workspace-exists-p "*leetcode*"))
    (+workspace-delete "*leetcode*")
    (+workspace/other)))
