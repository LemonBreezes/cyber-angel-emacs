;;; vanilla-emacs-configs/cape-tree-sitter-faces-bug.el -*- lexical-binding: t; -*-

;; Bootstrap straight
(defvar bootstrap-version)

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
                                    'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'tree-sitter)
(straight-use-package 'tree-sitter-langs)
(require 'tree-sitter-hl)

(require 'cc-mode)
(scratch-buffer)
(insert "int main() { return 0; // comment")
(c-mode)
(setq tree-sitter-hl-enable-query-region-extension t)
(tree-sitter-hl-mode)
(tree-sitter-hl--highlight-region (point-min) (point-max))
(message "get-text-property: %s | get-pos-property: %s"
         (get-text-property (point-max) 'face)
         (get-pos-property (point-max) 'face))
