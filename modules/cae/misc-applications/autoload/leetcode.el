;;; cae/misc-applications/autoload/leetcode.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-leetcode ()
  (interactive)
  (when (modulep! :ui workspaces)
    (progn (+workspace-switch cae-leetcode-workspace-name t)
           (set-persp-parameter 'dont-save-to-file t
                                (+workspace-get cae-leetcode-workspace-name))))
  (leetcode))

;;;###autoload
(defun cae-leetcode-quit ()
  (interactive)
  (leetcode-quit)
  (when (and (modulep! :ui workspaces)
             (+workspace-exists-p cae-leetcode-workspace-name))
    (+workspace-kill cae-leetcode-workspace-name)
    (+workspace/other)))

;;;###autoload
(defun cae-leetcode-soft-quit ()
  (interactive)
  (if (modulep! :ui workspaces)
      (+workspace/other)
    (quit-window)))
