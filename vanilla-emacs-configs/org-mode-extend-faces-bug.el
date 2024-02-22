;;; vanilla-emacs-configs/org-mode-extend-faces-bug.el -*- lexical-binding: t; -*-

(load-file (expand-file-name "default.el" (file-name-directory user-init-file)))

(require 'org)
(load-theme 'leuven t)

(dolist (face '(org-level-1 org-level-2 org-level-3 org-level-4 org-level-5
                org-level-6 org-level-7 org-level-8))
  (set-face-attribute face nil :extend t))

(setq org-fontify-whole-heading-line t)
(scratch-buffer)
(org-mode)
(insert "* Heading 1\n** Heading 2\n** Heading 3\n** Heading 4\n")
