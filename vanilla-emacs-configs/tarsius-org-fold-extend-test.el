;;; vanilla-emacs-configs/tarsius-org-fold-extend-test.el -*- lexical-binding: t; -*-

(add-to-list 'load-path "/home/st/.config/emacs/.local/straight/build-30.0.50/org/lisp/")
(setq org-fontify-whole-heading-line t)
(require 'org-loaddefs)
(load-theme 'leuven t)

(with-current-buffer (get-buffer-create "*demo*")
  (org-mode)
  (erase-buffer)
  (save-excursion
    (insert "* foo\n** bar\ntest\n** baz\ntest\n\n"))
  (pop-to-buffer (current-buffer)))
