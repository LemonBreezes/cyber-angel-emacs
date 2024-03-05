;;; vanilla-emacs-configs/tarsius-org-fold-extend-test.el -*- lexical-binding: t; -*-

(require 'cl-lib)
(setq load-path (remove-if (lambda (x) (string-match-p "org$" x)) load-path))
(add-to-list 'load-path "/home/st/.config/emacs/.local/straight/build-30.0.50/org/lisp/")
(setq org-fontify-whole-heading-line t)
(require 'org)
(load-theme 'leuven t)

(with-current-buffer (get-buffer-create "*demo*")
  (org-mode)
  (erase-buffer)
  (save-excursion
    (insert "* foo\n** bar\ntest\n** baz\ntest\n\n"))
  (pop-to-buffer (current-buffer)))
