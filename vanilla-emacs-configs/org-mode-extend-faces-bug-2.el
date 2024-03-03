;;; vanilla-emacs-configs/org-mode-extend-faces-bug-2.el -*- lexical-binding: t; -*-

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
(scratch-buffer)
(org-mode)
(insert "* Headline 1\n** Headline 2\n*** Headline 3\n**** Headline 4\n***** Headline 5\n****** Headline 6\n******* Headline 7\n******** Headline 8\n* Headline 1\n** Headline 2\n*** Headline 3\n**** Headline 4\n***** Headline 5\n****** Headline 6\n******* Headline 7\n******** Headline 8\n")
