;;; private/ai/autoload/ai.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-org-ai-on-buffer ()
  (interactive)
  (save-mark-and-excursion
    (mark-whole-buffer)
    (call-interactively #'org-ai-on-region)))

;;;###autoload
(defun cae-org-ai-on-region-or-buffer ()
  (interactive)
  (if (region-active-p)
      (call-interactively #'org-ai-on-region)
    (call-interactively #'cae-org-ai-on-buffer)))

;;;###autoload
(defun cae-ai-org-ai-kill-region-at-point ()
  (interactive)
  (call-interactively #'org-ai-kill-region-at-point)
  (forward-char -1)
  (let ((end (point))
        (s (buffer-substring-no-properties
            (point)
            (progn (skip-syntax-backward "\\s ") (point)))))
    (kill-append (concat s "\n") t)
    (delete-region (point) end)))

