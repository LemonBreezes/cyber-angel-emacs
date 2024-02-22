;;; vanilla-emacs-configs/cape-tree-sitter-faces-bug.el -*- lexical-binding: t; -*-

(let ((default-directory (file-name-directory load-file-name)))
  (when (file-exists-p (expand-file-name "default.el" default-directory))
    (load-file (expand-file-name "default.el" default-directory))))

(straight-use-package 'tree-sitter)
(straight-use-package 'tree-sitter-langs)

(require 'cc-mode)
(scratch-buffer)
(insert "int main() { return 0; // comment")
(goto-char (point-max))
(c-mode)
(tree-sitter-hl-mode)
(font-lock-fontify-buffer)
(message "get-text-property: %s | get-pos-property: %s"
         (get-text-property (point) 'face)
         (get-pos-property (point) 'face))
