;;; vanilla-emacs-configs/org-element-not-being-compiled.el -*- lexical-binding: t; -*-


(let ((default-directory (file-name-directory load-file-name)))
  (when (file-exists-p (expand-file-name "default.el" default-directory))
    (load-file (expand-file-name "default.el" default-directory))))

(straight-use-package 'org)
(straight-use-package 'vertico)

(require 'org-element)
(vertico-mode +1)
