;;; vanilla-emacs-configs/evil-mc-test.el -*- lexical-binding: t; -*-

(let ((default-directory (file-name-directory load-file-name)))
  (when (file-exists-p (expand-file-name "default.el" default-directory))
    (load-file (expand-file-name "default.el" default-directory))))

(straight-use-package 'evil-mc)

(require 'evil-mc)

(evil-mode +1)
