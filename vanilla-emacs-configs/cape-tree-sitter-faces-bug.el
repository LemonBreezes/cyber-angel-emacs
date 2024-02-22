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

(require 'cc-mode)
(scratch-buffer)
(insert "int main() { return 0; // comment")
(c-mode)
(tree-sitter-hl-mode)
(run-at-time 1 nil
             (lambda () (message "get-text-property: %s | get-pos-property: %s"
                                 (get-text-property (point-max) 'face)
                                 (get-pos-property (point-max) 'face))))
