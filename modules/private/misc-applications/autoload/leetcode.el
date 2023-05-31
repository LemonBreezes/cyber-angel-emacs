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

;;;###autoload (autoload '+leetcode-problems-hydra/body "private/misc-applications/autoload/leetcode" nil t)
(defhydra +leetcode-problems-hydra (:color pink :hint nil)
  ("<f6>" nil "Exit" :exit t)
  ("q" +workspace/other nil :exit t)
  ("Q" +leetcode-quit nil :exit t)
  )
