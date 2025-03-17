;;; cae/misc-applications/autoload/ai.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-claude-code ()
  (interactive)
  (require 'vterm)
  (let ((vterm-buffer-name (format "*claude:%s*" (doom-project-root)))
        (default-directory (or (doom-project-root) default-directory)))
    (if (get-buffer vterm-buffer-name)
        (pop-to-buffer vterm-buffer-name)
      (vterm-other-window vterm-buffer-name)
      (vterm-send-string "claude")
      (vterm-send-return))))
