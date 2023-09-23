;;; autoload/cae-org.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +org-insert-file-link ()
  "Insert a file link.  At the prompt, enter the filename."
  (interactive)
  (insert (format "[[%s]]" (org-link-complete-file))))
