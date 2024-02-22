;;; vanilla-emacs-configs/cape-tree-sitter-faces-bug.el -*- lexical-binding: t; -*-

(let ((default-directory (file-name-directory load-file-name)))
  (when (file-exists-p (expand-file-name "default.el" default-directory))
    (load-file (expand-file-name "default.el" default-directory))))
