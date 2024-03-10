;;; vanilla-emacs-configs/org-mode-extend-faces-bug-4.el -*- lexical-binding: t; -*-

(let ((default-directory (file-name-directory load-file-name)))
  (when (file-exists-p (expand-file-name "default.el" default-directory))
    (load-file (expand-file-name "default.el" default-directory))))

(straight-use-package 'org)

(require 'org)
(load-theme 'leuven t)

(dolist (face '(org-level-1 org-level-2 org-level-3 org-level-4 org-level-5
                org-level-6 org-level-7 org-level-8))
  (set-face-attribute face nil :extend t))

(setq org-fontify-whole-heading-line t)
(setq org-startup-folded t)

(defvar org-extend-faces-bug-4-file "/tmp/org-extend-faces-bug-4.org"
  "The file to be used for the bug demo.")

(with-temp-buffer
  (insert
   (string-join
    '("* First heading"
      "Body text of the first heading"
      ""
      "* Second heading"
      "Body text of the second heading"
      "* Last heading")
    "\n"))
  (write-file org-extend-faces-bug-4-file))

(find-file org-extend-faces-bug-4-file)
