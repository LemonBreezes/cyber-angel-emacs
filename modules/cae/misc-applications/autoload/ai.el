;;; cae/misc-applications/autoload/ai.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-claude-code ()
  (interactive)
  (let ((eat-buffer-name (format "*claude:%s*" (doom-project-root))))
    (eat-other-window "claude")))
