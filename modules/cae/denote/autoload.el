;;; cae/denote/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +denote-open-denote-directory ()
  (interactive)
  (dired denote-directory))

;;;###autoload
(defun +denote-find-note-file ()
  (interactive)
  (if (modulep! :completion vertico)
      (+vertico/consult-fd-or-find denote-directory ".*\\.org$ ")
    (let ((default-directory denote-directory))
      (call-interactively #'+default/find-file-under-here))))
