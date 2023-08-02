;;; private/helm/autoload/helm.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +helm-lazy-load ()
  (interactive)
  (require 'helm)
  (setq unread-command-events (list ?\C-x ?c))
  (setq which-key-inhibit t)
  (cae-which-key-show-map 'helm-command-map))
