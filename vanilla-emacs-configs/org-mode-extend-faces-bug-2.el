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
(insert "#+title:    :completion corfu
#+subtitle: Complete with cap(f), cape and a flying feather
#+created:  September 9, 2022
#+since:    3.0.0 (#7002)

* Description :unfold:
This module provides code completion, powered by [[doom-package:corfu]].

It is recommended to enable either this or [[doom-module::completion company]], in
case you desire pre-configured auto-completion. Corfu is much lighter weight and
focused, plus it's built on native Emacs functionality, whereas company is heavy
and highly non-native, but has some extra features and more maturity.

** Maintainers
- [[doom-user:][@LuigiPiucco]]

[[doom-contrib-maintainer:][Become a maintainer?]]
")
