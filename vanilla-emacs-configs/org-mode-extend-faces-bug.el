;;; vanilla-emacs-configs/org-mode-extend-faces-bug.el -*- lexical-binding: t; -*-

(load-file (expand-file-name "default.el" default-directory))

(require 'org)
(load-theme 'leuven t)

(dolist (face '(org-level-1 org-level-2 org-level-3 org-level-4 org-level-5
                org-level-6 org-level-7 org-level-8))
  (set-face-attribute face nil :extend t))

(scratch-buffer)
(org-mode)
;; insert an org document with 2 levels of headings and 4 headings total
(insert "* Heading 1\n** Heading 2\n** Heading 3\n** Heading 4\n")
