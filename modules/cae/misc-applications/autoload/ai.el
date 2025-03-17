;;; cae/misc-applications/autoload/ai.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-claude-code ()
  "Open a Claude AI terminal in a vterm buffer for the current project.
If a Claude buffer already exists for this project, switch to it."
  (interactive)
  (require 'vterm)
  (let* ((project-root (doom-project-root))
         (vterm-buffer-name (format "*claude:%s*" project-root))
         (default-directory (or project-root default-directory))
         (existing-buffer (get-buffer vterm-buffer-name)))
    (if existing-buffer
        (pop-to-buffer existing-buffer)
      (vterm-other-window vterm-buffer-name)
      (vterm-send-string "claude")
      (vterm-send-return))))
