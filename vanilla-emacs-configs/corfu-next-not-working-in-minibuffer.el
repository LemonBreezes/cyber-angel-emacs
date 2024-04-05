;;; vanilla-emacs-configs/corfu-next-not-working-in-minibuffer.el -*- lexical-binding: t; -*-

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

(straight-use-package 'corfu)
(straight-use-package 'cape)

(require 'corfu)
(require 'cape)
(minibuffer-with-setup-hook
    (lambda ()
      (add-hook 'completion-at-point-functions #'cape-dabbrev nil 'local)
      (corfu-mode +1))
  (read-from-minibuffer "Test: "))
