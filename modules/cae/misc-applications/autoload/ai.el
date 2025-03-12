;;; cae/misc-applications/autoload/ai.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-claude-code ()
  (interactive)
  (let ((vterm-buffer-name (format "*claude:%s*" (doom-project-root))))
    (vterm-other-window "claude")
    (vterm-send-string "claude")
    (vterm-send-return)))
