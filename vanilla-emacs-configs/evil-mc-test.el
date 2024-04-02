;;; vanilla-emacs-configs/evil-mc-test.el -*- lexical-binding: t; -*-

(straight-use-package 'evil-mc)

(require 'evil-mc)

(evil-mode +1)
(scratch-buffer)
(insert "hello world\nhello world\nhello world")
