;;; autoload/cae-ai.el -*- lexical-binding: t; -*-

(defun cae-org-ai-on-buffer ()
  (save-mark-and-excursion
    (mark-whole-buffer)
    (call-interactively #'org-ai-on-region)))
